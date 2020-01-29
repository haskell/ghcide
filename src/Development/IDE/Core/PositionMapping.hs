{-# LANGUAGE RankNTypes #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
module Development.IDE.Core.PositionMapping
  ( PositionMapping
  , toCurrentPosition
  , fromCurrentPosition
  , toCurrentRange
  , fromCurrentRange
  , toMapping
  -- newEnd and morph are mainly exposed for testing
  , newEnd
  , morph
  ) where

import qualified Data.Text as T
import Language.Haskell.LSP.Types
import Control.Lens

-- (Position -> Maybe Position, Position -> Maybe Position)
type PositionMapping = AnIso' (Maybe Position) (Maybe Position)

toCurrentPosition :: PositionMapping -> Position -> Maybe Position
toCurrentPosition mapping p = Just p ^. cloneIso mapping

fromCurrentPosition :: PositionMapping -> Position -> Maybe Position
fromCurrentPosition mapping p = Just p ^. from mapping

toCurrentRange :: PositionMapping -> Range -> Maybe Range
toCurrentRange mapping (Range a b) =
    Range <$> toCurrentPosition mapping a <*> toCurrentPosition mapping b

fromCurrentRange :: PositionMapping -> Range -> Maybe Range
fromCurrentRange mapping (Range a b) =
    Range <$> fromCurrentPosition mapping a <*> fromCurrentPosition mapping b

toMapping :: TextDocumentContentChangeEvent -> PositionMapping
toMapping (TextDocumentContentChangeEvent (Just (Range start end)) _ t) =
  iso (>>= morph start end (newEnd t start)) (>>= morph start (newEnd t start) end)
toMapping _ = id

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