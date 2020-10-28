{-# LANGUAGE RankNTypes #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

module Development.IDE.Spans.Documentation (
    getDocumentation
  , getDocumentationTryGhc
  , getDocumentationsTryGhc
  , DocMap
  , mkDocMap
  ) where

import Control.Concurrent.Extra
import           Control.Monad
import           Control.Monad.Extra (findM)
import           Data.Either
import           Data.Foldable
import           Data.List.Extra
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe
import qualified Data.Text as T
import           Development.IDE.Core.Compile
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error
import           Development.IDE.Spans.Common
import           Development.IDE.Core.RuleTypes
import           System.Directory
import           System.FilePath
import qualified Documentation.Haddock as H
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import           FastString
import           SrcLoc (RealLocated)
import           GhcMonad
import           Packages
import           Name
import           Language.Haskell.LSP.Types (getUri, filePathToUri)
import           TcRnTypes
import           ExtractDocs
import           NameEnv
import HscTypes (HscEnv(hsc_dflags))
import Control.Monad.Trans.Maybe
import Data.IORef
import NameCache
import Data.Char (isAlpha, isAlphaNum, isAscii, ord)

mkDocMap
  :: HscEnv
  -> [ParsedModule]
  -> RefMap
  -> TcGblEnv
  -> Var (HashMap FilePath (Maybe H.LinkEnv))
  -> IORef NameCache
  -> IO DocAndKindMap
mkDocMap env sources rm this_mod linkEnvs ideNc =
  do let (_ , DeclDocMap this_docs, _) = extractDocs this_mod
     d <- foldrM getDocs (mkNameEnv $ M.toList $ fmap (`SpanDocString` SpanDocUris Nothing Nothing) this_docs) names

     k <- foldrM getType (tcg_type_env this_mod) names
     pure $ DKMap d k
  where
    getDocs n map
      | maybe True (mod ==) $ nameModule_maybe n = pure map -- we already have the docs in this_docs, or they do not exist
      | otherwise = do
      doc <- getDocumentationTryGhc env mod sources n linkEnvs ideNc
      pure $ extendNameEnv map n doc
    getType n map
      | isTcOcc $ occName n = do
        kind <- lookupKind env mod n
        pure $ maybe map (extendNameEnv map n) kind
      | otherwise = pure map
    names = rights $ S.toList idents
    idents = M.keysSet rm
    mod = tcg_mod this_mod

lookupKind :: HscEnv -> Module -> Name -> IO (Maybe TyThing)
lookupKind env mod =
    fmap (either (const Nothing) id) . catchSrcErrors (hsc_dflags env) "span" . lookupName env mod

getDocumentationTryGhc :: HscEnv
  -> Module
  -> [ParsedModule]
  -> Name
  -> Var (HashMap FilePath (Maybe H.LinkEnv))
  -> IORef NameCache
  -> IO SpanDoc
getDocumentationTryGhc env mod deps n linkEnvs ideNc = head <$> getDocumentationsTryGhc env mod deps [n] linkEnvs ideNc

getDocumentationsTryGhc :: HscEnv
  -> Module
  -> [ParsedModule]
  -> [Name]
  -> Var (HashMap FilePath (Maybe H.LinkEnv))
  -> IORef NameCache
  -> IO [SpanDoc]
-- Interfaces are only generated for GHC >= 8.6.
-- In older versions, interface files do not embed Haddocks anyway
getDocumentationsTryGhc env mod sources names linkEnvs ideNc = do
  res <- catchSrcErrors (hsc_dflags env) "docs" $ getDocsBatch env mod names
  case res of
      Left _ -> mapM mkSpanDocText names
      Right res -> zipWithM unwrap res names
  where
    unwrap (Right (Just docs, _)) n = SpanDocString docs <$> getUris n
    unwrap _ n = mkSpanDocText n

    mkSpanDocText name =
      pure (SpanDocText (getDocumentation sources name)) <*> getUris name
   
    -- Get the uris to the documentation and source html pages if they exist
    getUris name = do
      let df = hsc_dflags env
      (docFu, srcFu) <-
        case nameModule_maybe name of
          Just mod -> do
            doc <- toFileUriText $ lookupHtmlDocForName df name linkEnvs ideNc
            src <- toFileUriText $ lookupSrcHtmlForModule df mod
            return (doc, src)
          Nothing -> pure (Nothing, Nothing)
      
      let docUri = (<> "#" <> anchorId name) <$> docFu
          srcUri = (<> "#" <> anchorId name) <$> srcFu

      return $ SpanDocUris docUri srcUri

    anchorId name = (T.pack . nameAnchorId . occName) name
    toFileUriText = (fmap . fmap) (getUri . filePathToUri)

getDocumentation
 :: HasSrcSpan name
 => [ParsedModule] -- ^ All of the possible modules it could be defined in.
 ->  name -- ^ The name you want documentation for.
 -> [T.Text]
-- This finds any documentation between the name you want
-- documentation for and the one before it. This is only an
-- approximately correct algorithm and there are easily constructed
-- cases where it will be wrong (if so then usually slightly but there
-- may be edge cases where it is very wrong).
-- TODO : Build a version of GHC exactprint to extract this information
-- more accurately.
getDocumentation sources targetName = fromMaybe [] $ do
  -- Find the module the target is defined in.
  targetNameSpan <- realSpan $ getLoc targetName
  tc <-
    find ((==) (Just $ srcSpanFile targetNameSpan) . annotationFileName)
      $ reverse sources -- TODO : Is reversing the list here really neccessary?

  -- Top level names bound by the module
  let bs = [ n | let L _ HsModule{hsmodDecls} = pm_parsed_source tc
           , L _ (ValD _ hsbind) <- hsmodDecls
           , Just n <- [name_of_bind hsbind]
           ]
  -- Sort the names' source spans.
  let sortedSpans = sortedNameSpans bs
  -- Now go ahead and extract the docs.
  let docs = ann tc
  nameInd <- elemIndex targetNameSpan sortedSpans
  let prevNameSpan =
        if nameInd >= 1
        then sortedSpans !! (nameInd - 1)
        else zeroSpan $ srcSpanFile targetNameSpan
  -- Annoyingly "-- |" documentation isn't annotated with a location,
  -- so you have to pull it out from the elements.
  pure
      $ docHeaders
      $ filter (\(L target _) -> isBetween target prevNameSpan targetNameSpan)
      $ mapMaybe (\(L l v) -> L <$> realSpan l <*> pure v)
      $ join
      $ M.elems
      docs
  where
    -- Get the name bound by a binding. We only concern ourselves with
    -- @FunBind@ (which covers functions and variables).
    name_of_bind :: HsBind GhcPs -> Maybe (Located RdrName)
    name_of_bind FunBind {fun_id} = Just fun_id
    name_of_bind _ = Nothing
    -- Get source spans from names, discard unhelpful spans, remove
    -- duplicates and sort.
    sortedNameSpans :: [Located RdrName] -> [RealSrcSpan]
    sortedNameSpans ls = nubSort (mapMaybe (realSpan . getLoc) ls)
    isBetween target before after = before <= target && target <= after
    ann = snd . pm_annotations
    annotationFileName :: ParsedModule -> Maybe FastString
    annotationFileName = fmap srcSpanFile . listToMaybe . realSpans . ann
    realSpans :: M.Map SrcSpan [Located a] -> [RealSrcSpan]
    realSpans =
        mapMaybe (realSpan . getLoc)
      . join
      . M.elems

-- | Shows this part of the documentation
docHeaders :: [RealLocated AnnotationComment]
           -> [T.Text]
docHeaders = mapMaybe (\(L _ x) -> wrk x)
  where
  wrk = \case
    -- When `Opt_Haddock` is enabled.
    AnnDocCommentNext s -> Just $ T.pack s
    -- When `Opt_KeepRawTokenStream` enabled.
    AnnLineComment s  -> if "-- |" `isPrefixOf` s
                            then Just $ T.pack s
                            else Nothing
    _ -> Nothing

-- These are taken from haskell-ide-engine's Haddock plugin

-- | Given a module finds the hyperlinked source @doc/html/src/Foo.Bar.Baz.html@ page.
-- An example for a cabal installed module:
-- @~/.cabal/store/ghc-8.10.1/vctr-0.12.1.2-98e2e861/share/doc/html/src/Data.Vector.Primitive.html@
lookupSrcHtmlForModule :: DynFlags -> Module -> IO (Maybe FilePath)
lookupSrcHtmlForModule df m = do
  -- try all directories
  let mfs = fmap (concatMap go) (lookupHtmlDir df ui)
  html <- findM doesFileExist (concat . maybeToList $ mfs)
  -- canonicalize located html to remove /../ indirection which can break some clients
  -- (vscode on Windows at least)
  traverse canonicalizePath html
  where
    go pkgDocDir = map (\modDocName -> pkgDocDir </> "src" </> modDocName <.> "html") mns
    ui = moduleUnitId m
    -- try to locate html file from most to least specific name e.g.
    --  first Language.Haskell.LSP.Types.Uri.html and Language-Haskell-LSP-Types-Uri.html
    --  then Language.Haskell.LSP.Types.html and Language-Haskell-LSP-Types.html etc.
    mns = do
      chunks <- (reverse . drop1 . inits . splitOn ".") $ (moduleNameString . moduleName) m
      -- The file might use "." or "-" as separator
      map (`intercalate` chunks) [".", "-"]

lookupHtmlDir :: DynFlags -> UnitId -> Maybe [FilePath]
lookupHtmlDir df ui =
  -- use haddockInterfaces instead of haddockHTMLs: GHC treats haddockHTMLs as URL not path 
  -- and therefore doesn't expand $topdir on Windows
  map takeDirectory . haddockInterfaces <$> lookupPackage df ui

-- TODO : clean up / use hlint, simplify plumbing
lookupHtmlDocForName :: DynFlags
  -> Name 
  -> Var (HashMap FilePath (Maybe H.LinkEnv))
  -> IORef NameCache
  -> IO (Maybe FilePath)
lookupHtmlDocForName df n linkEnvs ideNc = runMaybeT $ do
  (dir, mod) <- findNameHaddockDirAndModule df n linkEnvs ideNc
  html <- (MaybeT . liftIO) $ findM doesFileExist [dir </> moduleHtmlFile mod]
  -- canonicalize located html to remove /../ indirection which can break some clients
  -- (vscode on Windows at least)
  liftIO $ canonicalizePath html

findNameHaddockDirAndModule :: DynFlags
  -> Name
  -> Var (HashMap FilePath (Maybe H.LinkEnv))
  -> IORef NameCache
  -> MaybeT IO (FilePath, Module)
findNameHaddockDirAndModule df name linkEnvs ideNc = do
    fm <- (MaybeT . return) $ nameHaddockInterface_maybe name
    MaybeT $ findNameUri fm
  where
    nameHaddockInterface_maybe n = do
      m <- nameModule_maybe n
      p <- lookupPackage df $ moduleUnitId m
      i <- listToMaybe $ haddockInterfaces p
      return (i, n)

    readInterfaceFile fi =
      H.readInterfaceFile
        (readIORef ideNc, writeIORef ideNc)
        fi
#if MIN_GHC_API_VERSION(8,8,0)
        False -- don't bypass checks (default behavior before 8.8)
#endif
    readLinkEnvironment fi = do
      envs <- readVar linkEnvs
      case HMap.lookup fi envs of
        -- load and cache link environment
        Nothing -> 
          do
            ioe <- readInterfaceFile fi
            let mle =
                  case ioe of
                    Left _ -> Nothing
                    Right i -> Just $ H.ifLinkEnv i
            liftIO $ modifyVar_ linkEnvs (return . HMap.insert fi mle)
            return mle
        -- get cached
        Just mle ->
          return mle

    findNameUri (fi, name) = do
      let dir = takeDirectory fi
      exists <- doesFileExist fi
      if exists
        then do
          mle <- readLinkEnvironment fi
          case mle of
            Nothing -> return Nothing
            Just le ->
              return $ (dir,) <$> (M.lookup name le)
        else
          return Nothing


-- unfortunately haddock-api doesn't export Haddock.Utils,
-- Below is some blunt copy and paste file and anchor rendering logic (it's consistent from 8.6 to 8.10 so should just work)
-- TODO: export it upstream and reuse here

baseName :: ModuleName -> FilePath
baseName = map (\c -> if c == '.' then '-' else c) . moduleNameString


moduleHtmlFile :: Module -> FilePath
moduleHtmlFile mdl = baseName (moduleName mdl) ++ ".html" 

nameAnchorId :: OccName -> String
nameAnchorId name = makeAnchorId (prefix : ':' : occNameString name)
 where prefix | isValOcc name = 'v'
              | otherwise     = 't'

-- | Takes an arbitrary string and makes it a valid anchor ID. The mapping is
-- identity preserving.
makeAnchorId :: String -> String
makeAnchorId [] = []
makeAnchorId (f:r) = escape isAlpha f ++ concatMap (escape isLegal) r
  where
    escape p c | p c = [c]
               | otherwise = '-' : show (ord c) ++ "-"
    isLegal ':' = True
    isLegal '_' = True
    isLegal '.' = True
    isLegal c = isAscii c && isAlphaNum c
       -- NB: '-' is legal in IDs, but we use it as the escape char 
