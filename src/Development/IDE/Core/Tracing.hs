module Development.IDE.Core.Tracing where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Development.Shake
import           Development.IDE.Types.Options
import           Language.Haskell.LSP.Types
import           OpenTelemetry.Eventlog
import qualified Data.ByteString.Char8         as BS
import           Control.Concurrent.Extra
import           Control.Concurrent.Async
import GHC.DataSize
import qualified Data.HashMap.Strict as HMap
import           Data.Functor
import           System.Mem (performGC)

-- | Trace an action using OpenTelemetry. Adds various useful info into tags in the OpenTelemetry span.
otTraced :: BS.ByteString -> IO a -> IO a
otTraced name act = bracket (beginSpan name) endSpan (const act)

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

startTelemetry :: Show k => String -> Var (HMap.HashMap k v) -> IdeOTProfiling -> IO ()
startTelemetry _ _ (IdeOTProfiling False) = return ()
startTelemetry name valuesRef (IdeOTProfiling True) = do
  mapBytesInstrument <- mkValueObserver (BS.pack name <> " size_bytes")
  mapCountInstrument <- mkValueObserver (BS.pack name <> " count")

  regularly 500000 performGC
  _ <- regularly 10000 $ -- 100 times/s
    withSpan_ "Measure length" $
      readVar valuesRef
      <&> length
      >>= observe mapCountInstrument
  _ <- regularly 500000 $ -- 2 times/s (If it could run that fast)
    withSpan_ "Measure Memory" $
      readVar valuesRef
      >>= recursiveSize
      >>= observe mapBytesInstrument
  return ()

regularly :: Int -> IO () -> IO (Async ())
regularly delay act = async $ forever (act >> threadDelay delay)
