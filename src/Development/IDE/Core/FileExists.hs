{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Development.IDE.Core.FileExists
  ( fileExistsRules
  , modifyFileExists
  , getFileExists
  )
where

import           Control.Concurrent.Extra
import           Control.Exception
import           Control.Monad
import qualified Data.Aeson                    as A
import           Data.Binary
import qualified Data.ByteString.Lazy          as BS
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.Shake
import           Development.Shake.Classes
import           GHC.Generics
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Capabilities

-- | A map for tracking the file existence
type FileExistsMap = (Map NormalizedFilePath Bool)

-- | A wrapper around a mutable 'FileExistsMap'
newtype FileExistsMapVar = FileExistsMapVar (Var FileExistsMap)

instance IsIdeGlobal FileExistsMapVar

-- | Grab the current global value of 'FileExistsMap' without acquiring a dependency
getFileExistsMapUntracked :: Action FileExistsMap
getFileExistsMapUntracked = do
  FileExistsMapVar v <- getIdeGlobalAction
  liftIO $ readVar v

-- | Modify the global store of file exists
modifyFileExistsAction :: (FileExistsMap -> IO FileExistsMap) -> Action ()
modifyFileExistsAction f = do
  FileExistsMapVar var <- getIdeGlobalAction
  liftIO $ modifyVar_ var f

-- | Modify the global store of file exists
modifyFileExists :: IdeState -> [(NormalizedFilePath, Bool)] -> IO ()
modifyFileExists state changes = do
  FileExistsMapVar var <- getIdeGlobalState state
  changesMap           <- evaluate $ Map.fromList changes
  modifyVar_ var $ evaluate . Map.union changesMap
  let flushPreviousValues = do
        ShakeExtras { state } <- getShakeExtras
        liftIO $ mapM_ (resetValue state GetFileExists . fst) changes
  void $ shakeRun state [flushPreviousValues]

-------------------------------------------------------------------------------------

type instance RuleResult GetFileExists = Bool

data GetFileExists = GetFileExists
    deriving (Eq, Show, Typeable, Generic)

instance NFData   GetFileExists
instance Hashable GetFileExists
instance Binary   GetFileExists

-- | Returns True if the file exists
getFileExists :: NormalizedFilePath -> Action Bool
getFileExists fp = use_ GetFileExists fp

-- | Installs the 'getFileExists' rules.
--   Provides a fast implementation if client supports dynamic watched files.
--   Creates a global state as a side effect in that case.
fileExistsRules :: ClientCapabilities -> (NormalizedFilePath -> Action Bool) -> Rules ()
fileExistsRules ClientCapabilities{_workspace}
  | Just WorkspaceClientCapabilities{_didChangeWatchedFiles} <- _workspace
  , Just DidChangeWatchedFilesClientCapabilities{_dynamicRegistration} <- _didChangeWatchedFiles
  , Just True <- _dynamicRegistration
  = fileExistsRulesFast
  | otherwise = fileExistsRulesSlow

--   Requires an lsp client that provides WatchedFiles notifications.
fileExistsRulesFast :: (NormalizedFilePath -> Action Bool) -> Rules ()
fileExistsRulesFast getFileExists = do
  addIdeGlobal . FileExistsMapVar =<< liftIO (newVar [])
  defineEarlyCutoff $ \GetFileExists file -> do
    fileExistsMap <- getFileExistsMapUntracked
    let mbFilesWatched = Map.lookup file fileExistsMap
    case mbFilesWatched of
      Just fv -> pure (createKey fv, ([], Just fv))
      Nothing -> do
        exist                   <- getFileExists file
        ShakeExtras { eventer } <- getShakeExtras

        -- add a listener for VFS Create/Delete file events,
        -- taking the FileExistsMap lock to prevent race conditions
        -- that would lead to multiple listeners for the same path
        modifyFileExistsAction $ \x -> case Map.lookup file x of
          Just{}  -> return x
          Nothing -> do
            addListener eventer file
            return x
        pure (createKey exist, ([], Just exist))
 where
  createKey = Just . BS.toStrict . encode
  addListener eventer fp = do
    let
      req = RequestMessage "2.0" reqId ClientRegisterCapability regParams
      reqId        = IdString fpAsId
      fpAsId       = T.pack $ fromNormalizedFilePath fp
      regParams    = RegistrationParams (List [registration])
      registration = Registration fpAsId
                                  WorkspaceDidChangeWatchedFiles
                                  (Just (A.toJSON regOptions))
      regOptions =
        DidChangeWatchedFilesRegistrationOptions { watchers = List [watcher] }
      watcher = FileSystemWatcher { globPattern = fromNormalizedFilePath fp
                                  , kind        = Just 5 -- Create and Delete events only
                                  }

    void $ eventer $ ReqRegisterCapability req

fileExistsRulesSlow:: (NormalizedFilePath -> Action Bool) -> Rules ()
fileExistsRulesSlow getFileExists = do
  defineEarlyCutoff $ \GetFileExists file -> do
    alwaysRerun
    exist                   <- getFileExists file
    pure (createKey exist, ([], Just exist))
 where
  createKey = Just . BS.toStrict . encode


--------------------------------------------------------------------------------------------------
-- The message definitions below probably belong in haskell-lsp-types

data DidChangeWatchedFilesRegistrationOptions = DidChangeWatchedFilesRegistrationOptions
    { watchers :: List FileSystemWatcher
    }

instance A.ToJSON DidChangeWatchedFilesRegistrationOptions where
  toJSON DidChangeWatchedFilesRegistrationOptions {..} =
    A.object ["watchers" A..= watchers]

data FileSystemWatcher = FileSystemWatcher
    { -- | The glob pattern to watch.
      --   For details on glob pattern syntax, check the spec: https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#workspace_didChangeWatchedFiles
      globPattern :: String
        -- | The kind of event to subscribe to. Defaults to all.
        --   Defined as a bitmap of Create(1), Change(2), and Delete(4)
    , kind        :: Maybe Int
    }

instance A.ToJSON FileSystemWatcher where
  toJSON FileSystemWatcher {..} =
    A.object
      $  ["globPattern" A..= globPattern]
      ++ [ "kind" A..= x | Just x <- [kind] ]
