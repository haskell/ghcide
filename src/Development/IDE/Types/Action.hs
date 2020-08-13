module Development.IDE.Types.Action
  (DelayedAction(..)
  ,DelayedActionInternal
  ,ActionQueue
  ,newQueue
  ,pushQueue
  ,popQueue
  ,doneQueue
  ,peekInProgress
  )where

import Development.IDE.Types.Logger
import Development.Shake (Action)
import Control.Concurrent.STM (readTVar, modifyTVar, readTQueue, atomically, newTQueue, newTVar, writeTQueue, TQueue, STM, TVar)
import Data.Unique (Unique)
import Data.Hashable (Hashable(..))
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

data DelayedAction a = DelayedAction
  { uniqueID  :: Maybe Unique
  , actionName :: String -- ^ Name we use for debugging
  , actionPriority :: Priority -- ^ Priority with which to log the action
  , getAction :: Action a -- ^ The payload
  }
  deriving Functor

type DelayedActionInternal = DelayedAction ()

instance Eq (DelayedAction a) where

    a == b = uniqueID a == uniqueID b

instance Hashable (DelayedAction a) where
    hashWithSalt s = hashWithSalt s . uniqueID

instance Show (DelayedAction a) where
    show d = "DelayedAction: " ++ actionName d

------------------------------------------------------------------------------

data ActionQueue = ActionQueue {
    newActions :: TQueue DelayedActionInternal,
    inProgress :: TVar (HashSet DelayedActionInternal)
  }

newQueue :: IO ActionQueue
newQueue = atomically $ do
    newActions <- newTQueue
    inProgress <- newTVar mempty
    return ActionQueue{..}

pushQueue :: DelayedActionInternal -> ActionQueue -> STM ()
pushQueue act ActionQueue{..} = writeTQueue newActions act

-- | You must call 'doneQueue' to signal completion
popQueue :: ActionQueue -> STM DelayedActionInternal
popQueue ActionQueue{..} = do
    x <- readTQueue newActions
    modifyTVar inProgress (Set.insert x)
    return x

-- | Completely remove an action from the queue
doneQueue :: DelayedActionInternal -> ActionQueue -> STM ()
doneQueue x ActionQueue{..} =
    modifyTVar inProgress (Set.delete x)

peekInProgress :: ActionQueue -> STM [DelayedActionInternal]
peekInProgress ActionQueue{..} = Set.toList <$> readTVar inProgress
