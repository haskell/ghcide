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

data ReactorMessage c
  = ReactorNotification (IdeState -> LspM c ())
  | forall m. ReactorRequest SomeLspId (IdeState -> LspM c ()) (ResponseError -> LspM c ())

type ReactorChan c = Chan (ReactorMessage c)

requestHandler
  :: forall (m :: Method FromClient Request) c.
  ReactorChan c -> SMethod m -> (IdeState -> Handler m c) -> Handlers c
requestHandler chan m k = LSP.requestHandler m $ \msg@(RequestMessage{_id}) resp ->
  writeChan chan $ ReactorRequest (SomeLspId _id) (\st -> k st msg resp) (resp . Left)

notificationHandler
  :: forall (m :: Method FromClient Notification) c.
  ReactorChan c -> SMethod m -> (IdeState -> Handler m c) -> Handlers c
notificationHandler chan m k = LSP.notificationHandler m $ \msg ->
  writeChan chan $ ReactorNotification (\st -> k st msg)
