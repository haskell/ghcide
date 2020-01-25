module Development.IDE.Core.Abortable
  ( Abortable(..)
  , spawnWithContinuation
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Exception

-- | A computation that can be aborted.
data Abortable a = Abortable
  { abort :: IO () -- ^ Aborts the computation and blocks until it dies
  , wait  :: IO a  -- ^ Blocks for the computation result
  }

-- | Spawns an abortable computation
--   and runs the continuation when done.
spawnWithContinuation :: (Either SomeException a -> IO b) -> IO a -> IO (Abortable b)
spawnWithContinuation h act = do
  worker <- Async.asyncWithUnmask $ \restore -> try(restore act) >>= h
  let abort = Async.cancel worker
      wait  = Async.wait worker
  return Abortable {..}
