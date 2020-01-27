-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
module Development.IDE.Core.PositionMapping
  ( PositionMapping(..)
  , toCurrentRange
  , fromCurrentRange
  , applyChange
  , idMapping
  -- newEnd and morph are mainly exposed for testing
  , newEnd
  , morph
  ) where

import Control.Monad
import qualified Data.Text as T
import Language.Haskell.LSP.Types

data PositionMapping = PositionMapping
  { toCurrentPosition :: !(Position -> Maybe Position)
  , fromCurrentPosition :: !(Position -> Maybe Position)
  }

toCurrentRange :: PositionMapping -> Range -> Maybe Range
toCurrentRange mapping (Range a b) =
    Range <$> toCurrentPosition mapping a <*> toCurrentPosition mapping b

fromCurrentRange :: PositionMapping -> Range -> Maybe Range
fromCurrentRange mapping (Range a b) =
    Range <$> fromCurrentPosition mapping a <*> fromCurrentPosition mapping b

idMapping :: PositionMapping
idMapping = PositionMapping Just Just

applyChange :: PositionMapping -> TextDocumentContentChangeEvent -> PositionMapping
applyChange posMapping (TextDocumentContentChangeEvent (Just (Range start end)) _ t) = PositionMapping
    { toCurrentPosition = morph start end (newEnd t start) <=< toCurrentPosition posMapping
    , fromCurrentPosition = fromCurrentPosition posMapping <=< morph start (newEnd t start) end
    }
applyChange posMapping _ = posMapping

newEnd :: T.Text -> Position -> Position
newEnd t (Position startLine startColumn) = Position endLine endColumn where
  endLine = startLine + T.count "\n" t
  endColumn = T.length (T.takeWhileEnd (/= '\n') t)
    + if endLine == startLine then startColumn else 0

morph :: Position -> Position -> Position -> Position -> Maybe Position
morph start end@(Position endLine endColumn)
  (Position newEndLine newEndColumn) pos@(Position line column)
  | pos < start = Just $ Position line column
    -- Position is before the change and thereby unchanged.
  | pos > end =
    -- Position is after the change so increase line and column number
    -- as necessary.
    Just $ Position (line + newEndLine - endLine) $ column
      + if line == endLine then newEndColumn - endColumn else 0
  | otherwise = Nothing
  -- Position is in the region that was changed.