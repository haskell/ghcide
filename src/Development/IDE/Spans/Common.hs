{-# LANGUAGE CPP #-}
module Development.IDE.Spans.Common (
  showGhc
, listifyAllSpans
, listifyAllSpans'
, safeTyThingId
, safeTyThingType
, SpanDoc(..)
, emptySpanDoc
, spanDocToMarkdown
) where

import Data.Data
import qualified Data.Generics
import qualified Data.Text as T

import GHC
import ConLike
import Var
import DataCon
import Outputable
import DynFlags

import           Data.Char (isSpace)
import qualified Documentation.Haddock.Parser as H
import qualified Documentation.Haddock.Types as H

showGhc :: Outputable a => a -> String
showGhc = showPpr unsafeGlobalDynFlags

-- | Get ALL source spans in the source.
listifyAllSpans :: (Typeable a, Data m) => m -> [Located a]
listifyAllSpans tcs =
  Data.Generics.listify p tcs
  where p (L spn _) = isGoodSrcSpan spn
-- This is a version of `listifyAllSpans` specialized on picking out
-- patterns.  It comes about since GHC now defines `type LPat p = Pat
-- p` (no top-level locations).
listifyAllSpans' :: Typeable a
                   => TypecheckedSource -> [Pat a]
listifyAllSpans' tcs = Data.Generics.listify (const True) tcs

-- From haskell-ide-engine/src/Haskell/Ide/Engine/Support/HieExtras.hs

safeTyThingId :: TyThing -> Maybe Id
safeTyThingId (AnId i)                    = Just i
safeTyThingId (AConLike (RealDataCon dc)) = Just $ dataConWrapId dc
safeTyThingId _                           = Nothing

safeTyThingType :: TyThing -> Maybe Type
safeTyThingType thing
  | Just i <- safeTyThingId thing = Just (varType i)
safeTyThingType (ATyCon tycon)    = Just (tyConKind tycon)
safeTyThingType _                 = Nothing

-- Possible documentation for an element in the code
data SpanDoc
  = SpanDocString HsDocString
  | SpanDocText   [T.Text]
  deriving Show

emptySpanDoc :: SpanDoc
emptySpanDoc = SpanDocText []

spanDocToMarkdown :: SpanDoc -> [T.Text]
spanDocToMarkdown (SpanDocString docs)
  = [T.pack $ haddockToMarkdown $ H.toRegular $ H._doc $ H.parseParas Nothing $ unpackHDS docs]
spanDocToMarkdown (SpanDocText txt) = txt

-- Simple (and a bit hacky) conversion from Haddock markup to Markdown
haddockToMarkdown
  :: H.DocH String String -> String

haddockToMarkdown H.DocEmpty
  = ""
haddockToMarkdown (H.DocAppend d1 d2)
  = haddockToMarkdown d1 Prelude.<> haddockToMarkdown d2
haddockToMarkdown (H.DocString s)
  = s
haddockToMarkdown (H.DocParagraph p)
  = "\n\n" ++ haddockToMarkdown p
haddockToMarkdown (H.DocIdentifier i)
  = "`" ++ i ++ "`"
haddockToMarkdown (H.DocIdentifierUnchecked i)
  = "`" ++ i ++ "`"
haddockToMarkdown (H.DocModule i)
  = "`" ++ i ++ "`"
haddockToMarkdown (H.DocWarning w)
  = haddockToMarkdown w
haddockToMarkdown (H.DocEmphasis d)
  = "*" ++ haddockToMarkdown d ++ "*"
haddockToMarkdown (H.DocBold d)
  = "**" ++ haddockToMarkdown d ++ "**"
haddockToMarkdown (H.DocMonospaced d)
  = "`" ++ escapeBackticks (haddockToMarkdown d) ++ "`"
  where
    escapeBackticks "" = ""
    escapeBackticks ('`':ss) = '\\':'`':escapeBackticks ss
    escapeBackticks (s  :ss) = s:escapeBackticks ss
haddockToMarkdown (H.DocCodeBlock d)
  = "\n```haskell\n" ++ haddockToMarkdown d ++ "\n```\n"
haddockToMarkdown (H.DocExamples es)
  = "\n```haskell\n" ++ unlines (map exampleToMarkdown es) ++ "\n```\n"
  where
    exampleToMarkdown (H.Example expr result)
      = ">>> " ++ expr ++ "\n" ++ unlines result
haddockToMarkdown (H.DocHyperlink (H.Hyperlink url Nothing))
  = "<" ++ url ++ ">"
#if MIN_VERSION_haddock_library(1,8,0)
haddockToMarkdown (H.DocHyperlink (H.Hyperlink url (Just label)))
  = "[" ++ haddockToMarkdown label ++ "](" ++ url ++ ")"
#else
haddockToMarkdown (H.DocHyperlink (H.Hyperlink url (Just label)))
  = "[" ++ label ++ "](" ++ url ++ ")"
#endif
haddockToMarkdown (H.DocPic (H.Picture url Nothing))
  = "![](" ++ url ++ ")"
haddockToMarkdown (H.DocPic (H.Picture url (Just label)))
  = "![" ++ label ++ "](" ++ url ++ ")"
haddockToMarkdown (H.DocAName aname)
  = "[" ++ aname ++ "]:"
haddockToMarkdown (H.DocHeader (H.Header level title))
  = replicate level '#' ++ " " ++ haddockToMarkdown title

haddockToMarkdown (H.DocUnorderedList things)
  = '\n' : (unlines $ map (\thing -> "+ " ++ dropWhile isSpace (haddockToMarkdown thing)) things)
haddockToMarkdown (H.DocOrderedList things)
  = '\n' : (unlines $ map (\thing -> "1. " ++ dropWhile isSpace (haddockToMarkdown thing)) things)
haddockToMarkdown (H.DocDefList things)
  = '\n' : (unlines $ map (\(term, defn) -> "+ **" ++ haddockToMarkdown term ++ "**: " ++ haddockToMarkdown defn) things)

-- we cannot render math by default
haddockToMarkdown (H.DocMathInline _)
  = "*cannot render inline math formula*"
haddockToMarkdown (H.DocMathDisplay _)
  = "\n\n*cannot render display math formula*\n\n"

-- TODO: render tables
haddockToMarkdown (H.DocTable _t)
  = "\n\n*tables are not yet supported*\n\n"

-- things I don't really know how to handle
haddockToMarkdown (H.DocProperty _)
  = ""  -- don't really know what to do
