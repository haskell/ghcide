module Development.IDE.Core.Abortable
  ( Abortable(..)
  , startSimple
  , startWithContinuation
  ) where

import           Control.Concurrent.Extra
import           Control.Exception
import           Control.Monad

-- | A computation that can be aborted.
data Abortable a = Abortable
  { abort :: IO () -- ^ Aborts the computation and blocks until it dies
  , wait  :: IO a  -- ^ Blocks for the computation result
  }

-- | Spawns an abortable computation
startSimple :: IO a -> IO (Abortable (Either SomeException a))
startSimple act = do
  bar      <- newBarrier
  threadId <- forkFinally act $ signalBarrier bar
  let abort = killThread threadId >> void (waitBarrier bar)
      wait  = waitBarrier bar
  return Abortable { .. }

-- | Spawns an abortable computation
--   and runs the continuation when done.
--   The continuation runs masked.
startWithContinuation :: (Either SomeException a -> IO b) -> IO a -> IO (Abortable b)
startWithContinuation h act = do
  bar      <- newBarrier
  threadId <- forkFinally act (h >=> signalBarrier bar)
  let abort = killThread threadId >> void (waitBarrier bar)
      wait  = waitBarrier bar
  return Abortable { .. }
