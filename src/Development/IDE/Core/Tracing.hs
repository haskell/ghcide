{-# LANGUAGE DataKinds #-}
module Development.IDE.Core.Tracing
    ( otTracedHandler
    , otTracedAction
    , startTelemetry
    , measureMemory
    , getInstrumentCached
    , groupForObservableSharing
    )
where

import           Control.Seq
import           Development.Shake
import           Development.IDE.Types.Shake
import           Language.Haskell.LSP.Types
import           OpenTelemetry.Eventlog
import qualified Data.ByteString.Char8         as BS
import Control.Concurrent.Extra
import qualified Data.HashMap.Strict as HMap
import Control.Concurrent.Async
import Control.Monad
import Foreign.Storable (Storable(sizeOf))
import HeapSize (recursiveSize, runHeapsize)
import Development.IDE.Core.RuleTypes
import Debug.Trace
import Data.Dynamic (Dynamic)
import Data.IORef

-- | Trace a handler using OpenTelemetry. Adds various useful info into tags in the OpenTelemetry span.
otTracedHandler
    :: String -- ^ Message type
    -> String -- ^ Message label
    -> IO a
    -> IO a
otTracedHandler requestType label act =
  let !name =
        if null label
          then BS.pack requestType
          else BS.pack (requestType <> ":" <> show label)
   -- Add an event so all requests can be quickly seen in the viewer without searching
   in withSpan name (\sp -> addEvent sp "" (name <> " received") >> act)

-- | Trace a Shake action using opentelemetry.
otTracedAction
    :: Show k
    => k -- ^ The Action's Key
    -> NormalizedFilePath -- ^ Path to the file the action was run for
    -> Action a -- ^ The action
    -> Action a
otTracedAction key file act = actionBracket
    (do
        sp <- beginSpan (BS.pack (show key))
        setTag sp "File" (BS.pack $ fromNormalizedFilePath file)
        return sp
    )
    endSpan
    (const act)

startTelemetry :: Var Values -> IO ()
startTelemetry stateRef = do
    instrumentFor <- getInstrumentCached
    mapCountInstrument <- mkValueObserver "values map count"

    _ <- regularly 10000 $ -- 100 times/s
        withSpan_ "Measure length" $
        readVar stateRef
        >>= observe mapCountInstrument . length

    _ <- regularly (1 * seconds) $
        measureMemory groupForObservableSharing instrumentFor stateRef
    return ()
  where
        seconds = 1000000

        regularly :: Int -> IO () -> IO (Async ())
        regularly delay act = async $ forever (act >> threadDelay delay)

type OurValueObserver = Int -> IO ()

getInstrumentCached :: IO (Maybe Key -> IO OurValueObserver)
getInstrumentCached = do
    instrumentMap <- newVar HMap.empty
    mapBytesInstrument <- mkValueObserver "value map size_bytes"

    let instrumentFor k = HMap.lookup k <$> readVar instrumentMap >>= \case
            Nothing -> do
                instrument <- mkValueObserver (BS.pack (show k ++ " size_bytes"))
                modifyVar_ instrumentMap (return . HMap.insert k instrument)
                return $ observe instrument
            Just v -> return $ observe v
    return $ maybe (return $ observe mapBytesInstrument) instrumentFor

measureMemory :: [[Key]] -> (Maybe Key -> IO OurValueObserver) -> Var Values -> IO ()
measureMemory groups instrumentFor stateRef = withSpan_ "Measure Memory" $ do
    values <- readVar stateRef
    valuesSizeRef <- newIORef 0
    let !groupsOfGroupedValues = groupValues values
    traceIO "STARTING MEMORY PROFILING"
    forM_ groupsOfGroupedValues $ \groupedValues ->
        repeatUntilJust $ do
        traceIO (show $ map fst groupedValues)
        runHeapsize $
            forM_ groupedValues $ \(k,v) -> withSpan ("Measure " <> (BS.pack $ show k)) $ \sp -> do
            acc <- liftIO $ newIORef 0
            observe <- liftIO $ instrumentFor $ Just k
            mapM_ (recursiveSize >=> \x -> liftIO (modifyIORef' acc (+ x))) v
            size <- liftIO $ readIORef acc
            let !byteSize = sizeOf (undefined :: Word) * size
            setTag sp "size" (BS.pack (show byteSize ++ " bytes"))
            () <- liftIO $ observe byteSize
            liftIO $ modifyIORef' valuesSizeRef (+ byteSize)

    valuesSize <- readIORef valuesSizeRef
    observe <- instrumentFor Nothing
    observe valuesSize
    traceIO "MEMORY PROFILING COMPLETED"
    where
        blacklist = []

        groupValues :: Values -> [ [(Key, [Value Dynamic])] ]
        groupValues values =
            let groupedValues =
                    [ [ (k, vv)
                      | k <- groupKeys
                      , let vv = [ v | ((_,k'), v) <- HMap.toList values , k == k']
                      ]
                    | groupKeys <- groups
                    ]
                otherValues =
                    HMap.toList $
                    HMap.fromListWith (++)
                    [ (k, [v])
                    | ((_, k), v) <- HMap.toList values
                    , k `notElem` blacklist
                    , k `notElem` concat groups
                    ]
                !result = groupedValues <> map (:[]) otherValues
                -- force the spine of the nested lists
            in result `using` seqList (seqList (seqTuple2 r0 (seqList r0)))

groupForObservableSharing :: [[Key]]
groupForObservableSharing =
        -- Group keys in bundles to be analysed jointly (for sharing)
        -- Ideally all the keys would be analysed together, but if we try to do too much the GC will interrupt the analysis
            [
              [ Key GhcSessionDeps
              , Key GhcSession
              , Key TypeCheck
              , Key GetModIface
              , Key GetHieAst
              , Key GetModSummary
              , Key GetModSummaryWithoutTimestamps
              , Key GetDependencyInformation
              , Key GetModuleGraph
              , Key GenerateCore
              , Key GetParsedModule
              , Key GhcSessionIO
              ]
            ]

repeatUntilJust :: Monad m => m (Maybe b) -> m b
repeatUntilJust action = do
    res <- action
    case res of
        Nothing -> trace "RETRYING MEMORY PROFILING" $ repeatUntilJust action
        Just x -> return x
