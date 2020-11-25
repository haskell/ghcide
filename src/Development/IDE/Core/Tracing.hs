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

import           Control.Concurrent.Async       (Async, async)
import           Control.Concurrent.Extra       (Var, modifyVar_, newVar,
                                                 readVar, threadDelay)
import           Control.Monad                  (forM_, forever, (>=>))
import           Control.Seq                    (r0, seqList, seqTuple2, using)
import           Data.Dynamic                   (Dynamic)
import qualified Data.HashMap.Strict            as HMap
import           Data.IORef                     (modifyIORef', newIORef,
                                                 readIORef, writeIORef)
import           Data.String                    (IsString (fromString))
import           Development.IDE.Core.RuleTypes (GenerateCore (GenerateCore),
                                                 GetDependencyInformation (GetDependencyInformation),
                                                 GetHieAst (GetHieAst),
                                                 GetModIface (GetModIface),
                                                 GetModSummary (GetModSummary),
                                                 GetModSummaryWithoutTimestamps (GetModSummaryWithoutTimestamps),
                                                 GetModuleGraph (GetModuleGraph),
                                                 GetParsedModule (GetParsedModule),
                                                 GhcSession (GhcSession),
                                                 GhcSessionDeps (GhcSessionDeps),
                                                 GhcSessionIO (GhcSessionIO),
                                                 TypeCheck (TypeCheck))
import           Development.IDE.Types.Logger   (Logger, logDebug)
import           Development.IDE.Types.Shake    (Key (..), Value, Values)
import           Development.Shake              (Action, actionBracket, liftIO)
import           Foreign.Storable               (Storable (sizeOf))
import           HeapSize                       (recursiveSize, runHeapsize)
import           Language.Haskell.LSP.Types     (NormalizedFilePath,
                                                 fromNormalizedFilePath)
import           Numeric.Natural                (Natural)
import           OpenTelemetry.Eventlog         (addEvent, beginSpan, endSpan,
                                                 mkValueObserver, observe,
                                                 setTag, withSpan, withSpan_)

-- | Trace a handler using OpenTelemetry. Adds various useful info into tags in the OpenTelemetry span.
otTracedHandler
    :: String -- ^ Message type
    -> String -- ^ Message label
    -> IO a
    -> IO a
otTracedHandler requestType label act =
  let !name =
        if null label
          then requestType
          else requestType <> ":" <> show label
   -- Add an event so all requests can be quickly seen in the viewer without searching
   in withSpan (fromString name) (\sp -> addEvent sp "" (fromString $ name <> " received") >> act)

-- | Trace a Shake action using opentelemetry.
otTracedAction
    :: Show k
    => k -- ^ The Action's Key
    -> NormalizedFilePath -- ^ Path to the file the action was run for
    -> Action a -- ^ The action
    -> Action a
otTracedAction key file act = actionBracket
    (do
        sp <- beginSpan (fromString (show key))
        setTag sp "File" (fromString $ fromNormalizedFilePath file)
        return sp
    )
    endSpan
    (const act)

startTelemetry :: Logger -> Var Values -> IO ()
startTelemetry logger stateRef = do
    instrumentFor <- getInstrumentCached
    mapCountInstrument <- mkValueObserver "values map count"

    _ <- regularly 10000 $ -- 100 times/s
        withSpan_ "Measure length" $
        readVar stateRef
        >>= observe mapCountInstrument . length

    _ <- regularly (1 * seconds) $
        measureMemory logger groupForObservableSharing instrumentFor stateRef
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

    let instrumentFor k = do
          mb_inst <- HMap.lookup k <$> readVar instrumentMap
          case mb_inst of
            Nothing -> do
                instrument <- mkValueObserver (fromString (show k ++ " size_bytes"))
                modifyVar_ instrumentMap (return . HMap.insert k instrument)
                return $ observe instrument
            Just v -> return $ observe v
    return $ maybe (return $ observe mapBytesInstrument) instrumentFor

whenNothing :: IO () -> IO (Maybe a) -> IO ()
whenNothing act mb = mb >>= f
  where f Nothing = act
        f Just{}  = return ()

measureMemory :: Logger -> [[Key]] -> (Maybe Key -> IO OurValueObserver) -> Var Values -> IO ()
measureMemory logger groups instrumentFor stateRef = withSpan_ "Measure Memory" $ do
    values <- readVar stateRef
    valuesSizeRef <- newIORef Nothing
    let !groupsOfGroupedValues = groupValues values
    logDebug logger "STARTING MEMORY PROFILING"
    forM_ groupsOfGroupedValues $ \groupedValues ->
        whenNothing (writeIORef valuesSizeRef Nothing) $
        repeatUntilJust 3 $ do
        logDebug logger (fromString $ show $ map fst groupedValues)
        runHeapsize $
            forM_ groupedValues $ \(k,v) -> withSpan ("Measure " <> (fromString $ show k)) $ \sp -> do
            acc <- liftIO $ newIORef 0
            observe <- liftIO $ instrumentFor $ Just k
            mapM_ (recursiveSize >=> \x -> liftIO (modifyIORef' acc (+ x))) v
            size <- liftIO $ readIORef acc
            let !byteSize = sizeOf (undefined :: Word) * size
            setTag sp "size" (fromString (show byteSize ++ " bytes"))
            () <- liftIO $ observe byteSize
            liftIO $ modifyIORef' valuesSizeRef (fmap (+ byteSize))

    mbValuesSize <- readIORef valuesSizeRef
    case mbValuesSize of
        Just valuesSize -> do
            observe <- instrumentFor Nothing
            observe valuesSize
            logDebug logger "MEMORY PROFILING COMPLETED"
        Nothing ->
            logDebug logger "Memory profiling could not be completed: increase the size of your nursery (+RTS -Ax) and try again"

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

repeatUntilJust :: Monad m => Natural -> m (Maybe a) -> m (Maybe a)
repeatUntilJust 0 _ = return Nothing
repeatUntilJust nattempts action = do
    res <- action
    case res of
        Nothing -> repeatUntilJust (nattempts-1) action
        Just{}  -> return res
