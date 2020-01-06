-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | Gives information about symbols at a given point in DAML files.
-- These are all pure functions that should execute quickly.
module Development.IDE.Spans.AtPoint (
    atPoint
  , gotoDefinition
  ) where

import           Development.IDE.Spans.Documentation
import           Development.IDE.GHC.Error
import Development.IDE.GHC.Orphans()
import Development.IDE.Types.Location

-- DAML compiler and infrastructure
import Development.Shake
import Development.IDE.GHC.Util
import Development.IDE.GHC.Compat
import Development.IDE.Types.Options
import           Development.IDE.Spans.Type as SpanInfo

-- GHC API imports
import Avail
import DynFlags
import FastString
import Name
import Outputable hiding ((<>))
import SrcLoc

import Control.Monad.Extra
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import           Data.Maybe
import           Data.List
import qualified Data.Text as T

-- | Locate the definition of the name at a given position.
gotoDefinition
  :: MonadIO m
  => (FilePath -> m (Maybe HieFile))
  -> IdeOptions
  -> HscEnv
  -> [SpanInfo]
  -> Position
  -> m (Maybe Location)
gotoDefinition getHieFile ideOpts pkgState srcSpans pos =
  listToMaybe <$> locationsAtPoint getHieFile ideOpts pkgState pos srcSpans

-- | Synopsis for the name at a given position.
atPoint
  :: IdeOptions
  -> [TypecheckedModule]
  -> [SpanInfo]
  -> Position
  -> Maybe (Maybe Range, [T.Text])
atPoint IdeOptions{..} tcs srcSpans pos = do
    firstSpan <- listToMaybe $ deEmpasizeGeneratedEqShow $ spansAtPoint pos srcSpans
    return (Just (range firstSpan), hoverInfo firstSpan)
  where
    -- Hover info for types, classes, type variables
    hoverInfo SpanInfo{spaninfoType = Nothing , ..} =
       documentation <> (wrapLanguageSyntax <$> name <> kind) <> location
     where
       documentation = findDocumentation mbName
       name     = [maybe shouldNotHappen showName  mbName]
       location = [maybe shouldNotHappen definedAt mbName]
       kind     = [] -- TODO
       shouldNotHappen = "ghcide: did not expect a type level component without a name"
       mbName = getNameM spaninfoSource

    -- Hover info for values/data
    hoverInfo SpanInfo{spaninfoType = (Just typ), ..} =
       documentation <> (wrapLanguageSyntax <$> nameOrSource <> typeAnnotation) <> location
     where
       mbName = getNameM spaninfoSource
       documentation  = findDocumentation mbName
       typeAnnotation = [colon <> showName typ]
       nameOrSource   = [maybe literalSource qualifyNameIfPossible mbName]
       literalSource = "" -- TODO: literals: display (length-limited) source
       qualifyNameIfPossible name' = modulePrefix <> showName name'
         where modulePrefix = maybe "" (<> ".") (getModuleNameAsText name')
       location = [maybe "" definedAt mbName]

    findDocumentation = maybe [] (getDocumentation tcs)
    definedAt name = "**Defined " <> T.pack (showSDocUnsafe $ pprNameDefnLoc name) <> "**\n"

    range SpanInfo{..} = Range
      (Position spaninfoStartLine spaninfoStartCol)
      (Position spaninfoEndLine spaninfoEndCol)

    colon = if optNewColonConvention then ": " else ":: "
    wrapLanguageSyntax x = T.unlines [ "```" <> T.pack optLanguageSyntax, x, "```"]

    -- NOTE(RJR): This is a bit hacky.
    -- We don't want to show the user type signatures generated from Eq and Show
    -- instances, as they do not appear in the source program.
    -- However the user could have written an `==` or `show` function directly,
    -- in which case we still want to show information for that.
    -- Hence we just move such information later in the list of spans.
    deEmpasizeGeneratedEqShow :: [SpanInfo] -> [SpanInfo]
    deEmpasizeGeneratedEqShow = uncurry (++) . partition (not . isTypeclassDeclSpan)
    isTypeclassDeclSpan :: SpanInfo -> Bool
    isTypeclassDeclSpan spanInfo =
      case getNameM (spaninfoSource spanInfo) of
        Just name -> any (`isInfixOf` getOccString name) ["==", "showsPrec"]
        Nothing -> False

locationsAtPoint :: forall m . MonadIO m => (FilePath -> m (Maybe HieFile)) -> IdeOptions -> HscEnv -> Position -> [SpanInfo] -> m [Location]
locationsAtPoint getHieFile IdeOptions{..} pkgState pos =
    fmap (map srcSpanToLocation) . mapMaybeM (getSpan . spaninfoSource) . spansAtPoint pos
  where getSpan :: SpanSource -> m (Maybe SrcSpan)
        getSpan NoSource = pure Nothing
        getSpan (SpanS sp) = pure $ Just sp
        getSpan (Named name) = case nameSrcSpan name of
            sp@(RealSrcSpan _) -> pure $ Just sp
            sp@(UnhelpfulSpan _) -> runMaybeT $ do
                guard (sp /= wiredInSrcSpan)
                -- This case usually arises when the definition is in an external package.
                -- In this case the interface files contain garbage source spans
                -- so we instead read the .hie files to get useful source spans.
                let mod = nameModule name
                let unitId = moduleUnitId mod
                pkgConfig <- MaybeT $ pure $ lookupPackageConfig unitId pkgState
                hiePath <- MaybeT $ liftIO $ optLocateHieFile optPkgLocationOpts pkgConfig mod
                hieFile <- MaybeT $ getHieFile hiePath
                avail <- MaybeT $ pure $ listToMaybe (filterAvails (eqName name) $ hie_exports hieFile)
                srcPath <- MaybeT $ liftIO $ optLocateSrcFile optPkgLocationOpts pkgConfig mod
                -- The location will point to the source file used during compilation.
                -- This file might no longer exists and even if it does the path will be relative
                -- to the compilation directory which we don’t know.
                let span = setFileName srcPath $ nameSrcSpan $ availName avail
                pure span
        -- We ignore uniques and source spans and only compare the name and the module.
        eqName :: Name -> Name -> Bool
        eqName n n' = nameOccName n == nameOccName n' && nameModule n == nameModule n'
        setFileName f (RealSrcSpan span) = RealSrcSpan (span { srcSpanFile = mkFastString f })
        setFileName _ span@(UnhelpfulSpan _) = span

-- | Filter out spans which do not enclose a given point
spansAtPoint :: Position -> [SpanInfo] -> [SpanInfo]
spansAtPoint pos = filter atp where
  line = _line pos
  cha = _character pos
  atp SpanInfo{..} =
      startsBeforePosition && endsAfterPosition
    where
      startLineCmp = compare spaninfoStartLine line
      endLineCmp   = compare spaninfoEndLine   line

      startsBeforePosition = startLineCmp == LT || (startLineCmp == EQ && spaninfoStartCol <= cha)
                                              -- The end col points to the column after the
                                              -- last character so we use > instead of >=
      endsAfterPosition = endLineCmp == GT || (endLineCmp == EQ && spaninfoEndCol > cha)

showName :: Outputable a => a -> T.Text
showName = T.pack . prettyprint
  where
    prettyprint x = renderWithStyle unsafeGlobalDynFlags (ppr x) style
    style = mkUserStyle unsafeGlobalDynFlags neverQualify AllTheWay

getModuleNameAsText :: Name -> Maybe T.Text
getModuleNameAsText n = do
  m <- nameModule_maybe n
  return . T.pack . moduleNameString $ moduleName m
