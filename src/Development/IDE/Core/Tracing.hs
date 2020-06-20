module Development.IDE.Core.Tracing (otTraced, otTracedAction) where

import           Development.Shake
import qualified Data.ByteString.Char8         as BS
import           Language.Haskell.LSP.Types
import           OpenTelemetry.Eventlog
import           Control.Exception
import           Control.Concurrent.Async
import           Control.Concurrent
import           Control.Monad

-- | Trace an action using OpenTelemetry. Adds various useful info into tags in the OpenTelemetry span.
otTraced :: String -> IO a -> IO a
otTraced name act = bracket (beginSpan name) endSpan (const act)

-- | Trace a Shake action using opentelemetry.
otTracedAction
    :: Show k
    => k -- ^ The Action's Key
    -> NormalizedFilePath -- ^ Path to the file the action was run for
    -> Action a -- ^ The action
    -> Action a
otTracedAction key file act = actionBracket
    (do
        sp <- beginSpan (show key)
        setTag sp "File" (BS.pack $ fromNormalizedFilePath file)
        return sp
    )
    endSpan
    (const act)

regularly :: Int -> IO () -> IO (Async ())
regularly delay act = async $ forever (threadDelay delay >> act)
