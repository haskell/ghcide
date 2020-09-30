{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
-- | A plugin that adds custom messages for use in tests
module Development.IDE.Plugin.Test where

import Control.Monad.STM
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text as T
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat
import Development.IDE.GHC.Util (HscEnvEq(hscEnv))
import Development.IDE.Plugin
import Development.IDE.LSP.Server
import Development.IDE.Types.Action
import GHC.Generics (Generic)
import GhcPlugins (HscEnv(hsc_dflags))
import Language.Haskell.LSP.Core hiding (requestHandler, notificationHandler)
import Language.Haskell.LSP.Types
import System.Time.Extra
import Development.IDE.Core.RuleTypes
import Control.Monad

newtype BlockSeconds = BlockSeconds Seconds           -- ^ :: Null
  deriving Generic
  deriving anyclass (FromJSON, ToJSON)

newtype GetInterfaceFilesDir = GetInterfaceFilesDir FilePath  -- ^ :: String
  deriving Generic
  deriving anyclass (FromJSON, ToJSON)

data GetShakeSessionQueueCount = GetShakeSessionQueueCount -- ^ :: Number
  deriving Generic
  deriving anyclass (FromJSON, ToJSON)

data WaitForShakeQueue = WaitForShakeQueue -- ^ Block until the Shake queue is empty. Returns Null
  deriving Generic
  deriving anyclass (FromJSON, ToJSON)

plugin :: Plugin c
plugin = Plugin {
    pluginRules = return (),
    pluginHandlers = customRequestHandler
}

customRequestHandler ::  Handlers (ServerM c)
customRequestHandler = mconcat
  [ requestHandler (SCustomMethod "ghcide/blocking/request") $ \_s (RequestMessage _ _ _ params) k -> case fromJSON params of
      Success (BlockSeconds secs) -> do
        sendNotification (SCustomMethod "ghcide/blocking/notification") $
          toJSON secs
        liftIO $ sleep secs
        k (Right Null)
      Error err -> k (Left $ ResponseError InvalidParams (T.pack err) Nothing)
  , requestHandler (SCustomMethod "ghcide/hidir") $ \s (RequestMessage _ _ _ params) k -> case fromJSON params of
      Success (GetInterfaceFilesDir fp) -> do
        let nfp = toNormalizedFilePath fp
        sess <- liftIO $ runAction "Test - GhcSession" s $ use_ GhcSession nfp
        let hiPath = hiDir $ hsc_dflags $ hscEnv sess
        k $ Right (toJSON hiPath)
      Error err -> k (Left $ ResponseError InvalidParams (T.pack err) Nothing)
  , requestHandler (SCustomMethod "ghcide/queue/count") $ \s (RequestMessage _ _ _ params) k -> case fromJSON params of
      Success GetShakeSessionQueueCount -> do
        n <- liftIO $ atomically $ countQueue $ actionQueue $ shakeExtras s
        k $ Right (toJSON n)
      Error err -> k (Left $ ResponseError InvalidParams (T.pack err) Nothing)
  , requestHandler (SCustomMethod "ghcide/blocking/queue") $ \s (RequestMessage _ _ _ params) k -> case fromJSON params of
      Success WaitForShakeQueue -> do
        liftIO $ atomically $ do
            n <- countQueue $ actionQueue $ shakeExtras s
            when (n>0) retry
        k $ Right Null
      Error err -> k (Left $ ResponseError InvalidParams (T.pack err) Nothing)
 ]

