{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
-- | A plugin that adds custom messages for use in tests
module Development.IDE.Plugin.Test (TestRequest(..), plugin) where

import Control.Monad.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat
import Development.IDE.GHC.Util (HscEnvEq(hscEnv))
import Development.IDE.LSP.Server
import Development.IDE.Plugin
import Development.IDE.Types.Action
import GHC.Generics (Generic)
import GhcPlugins (HscEnv(hsc_dflags))
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Types
import System.Time.Extra
import Development.IDE.Core.RuleTypes
import Control.Monad

data TestRequest
    = BlockSeconds Seconds           -- ^ :: Null
    | GetInterfaceFilesDir FilePath  -- ^ :: String
    | GetShakeSessionQueueCount      -- ^ :: Number
    | WaitForShakeQueue
      -- ^ Block until the Shake queue is empty. Returns Null
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

plugin :: Plugin c
plugin = Plugin {
    pluginRules = return (),
    pluginHandlers = \ide m -> case m of
      -- TODO
      SCustomMethod m -> undefined -- Just $ requestHandler ide
      _ -> Nothing
}

requestHandler ::  IdeState
                -> TestRequest
                -> LspM c (Either ResponseError Value)
requestHandler _ (BlockSeconds secs) = do
    sendNotification (SCustomMethod "ghcide/blocking/request") $
      toJSON secs
    liftIO $ sleep secs
    return (Right Null)
requestHandler s (GetInterfaceFilesDir fp) = liftIO $ do
    let nfp = toNormalizedFilePath fp
    sess <- runAction "Test - GhcSession" s $ use_ GhcSession nfp
    let hiPath = hiDir $ hsc_dflags $ hscEnv sess
    return $ Right (toJSON hiPath)
requestHandler s GetShakeSessionQueueCount = liftIO $ do
    n <- atomically $ countQueue $ actionQueue $ shakeExtras s
    return $ Right (toJSON n)
requestHandler s WaitForShakeQueue = liftIO $ do
    atomically $ do
        n <- countQueue $ actionQueue $ shakeExtras s
        when (n>0) retry
    return $ Right Null

