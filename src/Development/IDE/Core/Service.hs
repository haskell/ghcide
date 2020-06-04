-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Service(
    getIdeOptions,
    IdeState, initialise, shutdown,
    runAction,
    runActionSync,
    writeProfile,
    getDiagnostics, unsafeClearDiagnostics,
    ideLogger,
    updatePositionMapping,
    ) where

import Data.Maybe
import Development.IDE.Types.Options (IdeOptions(..))
import Control.Monad
import Development.IDE.Core.Debouncer
import           Development.IDE.Core.FileStore  (VFSHandle, fileStoreRules)
import           Development.IDE.Core.FileExists (fileExistsRules)
import           Development.IDE.Core.OfInterest
import Development.IDE.Types.Logger
import           Development.Shake
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as LSP

import           Development.IDE.Core.Shake



newtype GlobalIdeOptions = GlobalIdeOptions IdeOptions
instance IsIdeGlobal GlobalIdeOptions

------------------------------------------------------------
-- Exposed API

-- | Initialise the Compiler Service.
initialise :: LSP.ClientCapabilities
           -> Rules ()
           -> IO LSP.LspId
           -> (LSP.FromServerMessage -> IO ())
           -> Logger
           -> Debouncer LSP.NormalizedUri
           -> IdeOptions
           -> VFSHandle
           -> IO IdeState
initialise caps mainRule getLspId toDiags logger debouncer options vfs =
    shakeOpen
        getLspId
        toDiags
        logger
        debouncer
        (optShakeProfiling options)
        (optReportProgress options)
        (optTesting options)
        shakeOptions
          { shakeThreads = optThreads options
          , shakeFiles   = fromMaybe "/dev/null" (optShakeFiles options)
          } $ do
            addIdeGlobal $ GlobalIdeOptions options
            fileStoreRules vfs
            ofInterestRules
            fileExistsRules getLspId caps vfs
            mainRule

writeProfile :: IdeState -> FilePath -> IO ()
writeProfile = shakeProfile

-- | Shutdown the Compiler Service.
shutdown :: IdeState -> IO ()
shutdown = shakeShut

-- This will return as soon as the result of the action is
-- available.  There might still be other rules running at this point,
-- e.g., the ofInterestRule.
runAction :: IdeState -> Action a -> IO a
runAction ide action = join $ shakeEnqueue ide action

-- | `runActionSync` is similar to `runAction` but it will
-- wait for all rules (so in particular the `ofInterestRule`) to
-- finish running. This is mainly useful in tests, where you want
-- to wait for all rules to fire so you can check diagnostics.
runActionSync :: IdeState -> Action a -> IO a
runActionSync ide act = join $ shakeEnqueue ide act

getIdeOptions :: Action IdeOptions
getIdeOptions = do
    GlobalIdeOptions x <- getIdeGlobalAction
    return x
