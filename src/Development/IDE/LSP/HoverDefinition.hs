-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

-- | Display information on hover.
module Development.IDE.LSP.HoverDefinition
    ( setIDEHandlers
    -- * For haskell-language-server
    , hover
    , gotoDefinition
    , gotoTypeDefinition
    ) where

import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.LSP.Server
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import qualified Language.Haskell.LSP.Core       as LSP
import           Language.Haskell.LSP.Types

import qualified Data.Text as T

gotoDefinition :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError (ResponseParams TextDocumentDefinition))
hover          :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError (Maybe Hover))
gotoTypeDefinition :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError (ResponseParams TextDocumentTypeDefinition))
documentHighlight :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError (List DocumentHighlight))
gotoDefinition = request "Definition" getDefinition (InR $ InL $ List []) InL
gotoTypeDefinition = request "TypeDefinition" getTypeDefinition (InR $ InL $ List []) (InR . InL . List)
hover          = request "Hover"      getAtPoint     Nothing      foundHover
documentHighlight = request "DocumentHighlight" highlightAtPoint (List []) List

foundHover :: (Maybe Range, [T.Text]) -> Maybe Hover
foundHover (mbRange, contents) =
  Just $ Hover (HoverContents $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator contents) mbRange

setIdeHandlers :: IdeState -> LSP.Handlers c
setIdeHandlers ide STextDocumentDefinition = Just $ \(RequestMessage _ _ _ params) k ->
  k =<< gotoDefinition ide params
setIdeHandlers ide STextDocumentHover = Just $ \(RequestMessage _ _ _ params) k ->
  k =<< hover ide params
setIdeHandlers ide STextDocumentTypeDefinition = Just $ \(RequestMessage _ _ _ params) k ->
  k =<< gotoTypeDefinition ide params
setIdeHandlers ide STextDocumentDocumentHighlight = Just $ \(RequestMessage _ _ _ params) k ->
  k =<< documentHighlight ide params
setIdeHandlers _ _ = Nothing

-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> IdeAction (Maybe a))
  -> b
  -> (a -> b)
  -> IdeState
  -> TextDocumentPositionParams
  -> IO (Either ResponseError b)
request label getResults notFound found ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest label getResults ide pos path
        Nothing   -> pure Nothing
    pure $ Right $ maybe notFound found mbResult

logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> IdeAction b) -> IdeState -> Position -> String -> IO b
logAndRunRequest label getResults ide pos path = do
  let filePath = toNormalizedFilePath' path
  logInfo (ideLogger ide) $
    label <> " request at position " <> T.pack (showPosition pos) <>
    " in file: " <> T.pack path
  runIdeAction (T.unpack label) (shakeExtras ide) (getResults filePath pos)
