module Development.IDE.Spans.Common (
  showGhc
, listifyAllSpans
, listifyAllSpans'
, safeTyThingId
, safeTyThingType
) where

import Data.Data
import qualified Data.Generics

import GHC
import ConLike
import Var
import DataCon
import Outputable
import DynFlags


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