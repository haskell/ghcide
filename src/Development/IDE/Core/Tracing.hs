module Development.IDE.Core.Tracing
    ( otTraced
    , otTracedHandler
    , otTracedAction
    )
where

import           Control.Exception
import           Development.Shake
import           Language.Haskell.LSP.Types
import           OpenTelemetry.Eventlog
import qualified Data.ByteString.Char8         as BS

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
