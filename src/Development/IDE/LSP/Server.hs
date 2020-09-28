-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
module Development.IDE.LSP.Server where
--   ( WithMessage(..)
--   , PartialHandlers(..)
--   ) where


import Data.Default

import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Core
import Development.IDE.Core.Service

-- data WithMessage c = WithMessage
--     {withResponse :: forall m req resp . (Show m, Show req) =>
--         (ResponseMessage resp -> LSP.FromServerMessage) -> -- how to wrap a response
--         (IdeState -> req -> LspT c IO (Either ResponseError resp)) -> -- actual work
--         Maybe (LSP.Handler (RequestMessage m req resp))
--     ,withNotification :: forall m req . (Show m, Show req) =>
--         Maybe (LSP.Handler (NotificationMessage m req)) -> -- old notification handler
--         (LSP.LspFuncs c -> IdeState -> req -> IO ()) -> -- actual work
--         Maybe (LSP.Handler (NotificationMessage m req))
--     ,withResponseAndRequest :: forall m rm req resp newReqParams newReqBody .
--         (Show m, Show rm, Show req, Show newReqParams, Show newReqBody) =>
--         (ResponseMessage resp -> LSP.FromServerMessage) -> -- how to wrap a response
--         (RequestMessage rm newReqParams newReqBody -> LSP.FromServerMessage) -> -- how to wrap the additional req
--         (LSP.LspFuncs c -> IdeState -> req -> IO (Either ResponseError resp, Maybe (rm, newReqParams))) -> -- actual work
--         Maybe (LSP.Handler (RequestMessage m req resp))
--     , withInitialize :: (LSP.LspFuncs c -> IdeState -> InitializeParams -> IO ())
--                      -> Maybe (Handler c InitializeRequest)
--     }

-- newtype PartialHandlers c = PartialHandlers (WithMessage c -> Handlers c -> IO Handlers c)

-- instance Default (PartialHandlers c) where
--     def = PartialHandlers $ \_ x -> pure x

-- instance Semigroup (PartialHandlers c) where
--     PartialHandlers a <> PartialHandlers b = PartialHandlers $ \w x -> a w x >>= b w

-- instance Monoid (PartialHandlers c) where
--     mempty = def
