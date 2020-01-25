module Development.IDE.Core.Abortable
  ( Abortable(..)
  , spawnSimple
  , spawnWithContinuation
  , spawnWithContinuation2
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.Extra
import           Control.Exception
import           Control.Monad

-- | A computation that can be aborted.
data Abortable a = Abortable
  { abort :: IO () -- ^ Aborts the computation and blocks until it dies
  , wait  :: IO a  -- ^ Blocks for the computation result
  }

-- | Spawns an abortable computation
spawnSimple :: IO a -> IO (Abortable (Either SomeException a))
spawnSimple act = do
  bar      <- newBarrier
  threadId <- forkFinally act $ signalBarrier bar
  let abort = killThread threadId >> void (waitBarrier bar)
      wait  = waitBarrier bar
  return Abortable { .. }

-- | Spawns an abortable computation
--   and runs the continuation when done.
--   The continuation runs masked.
spawnWithContinuation :: (Either SomeException a -> IO b) -> IO a -> IO (Abortable b)
spawnWithContinuation h act = do
  bar      <- newBarrier
  threadId <- forkFinally act (h >=> signalBarrier bar)
  let abort = killThread threadId >> void (waitBarrier bar)
      wait  = waitBarrier bar
  return Abortable { .. }

-- | Spawns an abortable computation
--   and runs the continuation when done.
spawnWithContinuation2 :: (Either SomeException a -> IO b) -> IO a -> IO (Abortable b)
spawnWithContinuation2 h act = do
  worker <- Async.asyncWithUnmask $ \restore -> try(restore act) >>= h
  let abort = Async.cancel worker
      wait  = Async.wait worker
  return Abortable {..}
