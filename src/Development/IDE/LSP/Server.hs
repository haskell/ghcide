-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Development.IDE.LSP.Server where

import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Core as LSP
import           Language.Haskell.LSP.Core (Handlers, LspM, Handler)
import Development.IDE.Core.Shake
import UnliftIO.Chan
import Control.Monad.Reader

data ReactorMessage
  = ReactorNotification (IO ())
  | ReactorRequest SomeLspId (IO ()) (ResponseError -> IO ())

type ReactorChan = Chan ReactorMessage
type ServerM c = ReaderT (ReactorChan, IdeState) (LspM c)

requestHandler
  :: forall (m :: Method FromClient Request) c.
     SMethod m
  -> (IdeState -> Handler (LspM c) m)
  -> Handlers (ServerM c)
requestHandler m k = LSP.requestHandler m $ \msg@(RequestMessage{_id}) resp -> do
  st@(chan,ide) <- ask
  env <- LSP.getLspEnv
  let resp' = flip runReaderT st . resp
  writeChan chan $ ReactorRequest (SomeLspId _id) (LSP.runLspT env $ k ide msg resp') (LSP.runLspT env . resp' . Left)

notificationHandler
  :: forall (m :: Method FromClient Notification) c.
     SMethod m
  -> (IdeState -> Handler (LspM c) m)
  -> Handlers (ServerM c)
notificationHandler m k = LSP.notificationHandler m $ \msg -> do
  (chan,ide) <- ask
  env <- LSP.getLspEnv
  writeChan chan $ ReactorNotification (LSP.runLspT env $ k ide msg)
