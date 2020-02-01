-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms       #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Rules(
    IdeState, GetDependencies(..), GetParsedModule(..), TransitiveDependencies(..),
    Priority(..), GhcSessionIO(..), GhcSessionFun(..),
    priorityTypeCheck,
    priorityGenerateCore,
    priorityFilesOfInterest,
    runAction, useE, useNoFileE, usesE,
    toIdeResult, defineNoFile,
    mainRule,
    getAtPoint,
    getDefinition,
    getDependencies,
    getParsedModule,
    generateCore,
    ) where

import Fingerprint

import Control.Exception
import Data.Binary
import Data.Bifunctor (second)
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Development.IDE.Core.Compile
import Development.IDE.Core.OfInterest
import Development.IDE.Types.Options
import Development.IDE.Spans.Calculate
import Development.IDE.Import.DependencyInformation
import Development.IDE.Import.FindImports
import           Development.IDE.Core.FileExists
import           Development.IDE.Core.FileStore        (getFileContents)
import           Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.Types.Logger (logDebug)
import Development.IDE.GHC.Compat hiding (parseModule, typecheckModule)
import Development.IDE.GHC.Util
import Data.Coerce
import Data.Either.Extra
import Data.Maybe
import           Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Ord
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import qualified Data.Text.Encoding                       as TE
import           Development.IDE.GHC.Error
import           Development.Shake                        hiding (Diagnostic)
import Development.IDE.Core.RuleTypes
import Development.IDE.Spans.Type

import qualified GHC.LanguageExtensions as LangExt
import HscTypes
import DynFlags (xopt)
import GHC.Generics(Generic)

import qualified Development.IDE.Spans.AtPoint as AtPoint
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.Shake.Classes

-- | This is useful for rules to convert rules that can only produce errors or
-- a result into the more general IdeResult type that supports producing
-- warnings while also producing a result.
toIdeResult :: Either [FileDiagnostic] v -> IdeResult v
toIdeResult = either (, Nothing) (([],) . Just)

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
useE :: IdeRule k v => k -> NormalizedFilePath -> MaybeT Action v
useE k = MaybeT . use k

useNoFileE :: IdeRule k v => k -> MaybeT Action v
useNoFileE k = useE k ""

usesE :: IdeRule k v => k -> [NormalizedFilePath] -> MaybeT Action [v]
usesE k = MaybeT . fmap sequence . uses k

defineNoFile :: IdeRule k v => (k -> Action v) -> Rules ()
defineNoFile f = define $ \k file -> do
    if file == "" then do res <- f k; return ([], Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"


------------------------------------------------------------
-- Exposed API

-- | Get all transitive file dependencies of a given module.
-- Does not include the file itself.
getDependencies :: NormalizedFilePath -> Action (Maybe [NormalizedFilePath])
getDependencies file = fmap transitiveModuleDeps <$> use GetDependencies file

-- | Try to get hover text for the name under point.
getAtPoint :: NormalizedFilePath -> Position -> Action (Maybe (Maybe Range, [T.Text]))
getAtPoint file pos = fmap join $ runMaybeT $ do
  opts <- lift getIdeOptions
  spans <- useE GetSpanInfo file
  return $ AtPoint.atPoint opts spans pos

-- | Goto Definition.
getDefinition :: NormalizedFilePath -> Position -> Action (Maybe Location)
getDefinition file pos = fmap join $ runMaybeT $ do
    opts <- lift getIdeOptions
    spans <- useE GetSpanInfo file
    lift $ AtPoint.gotoDefinition (getHieFile file) opts (spansExprs spans) pos

getHieFile
  :: NormalizedFilePath -- ^ file we're editing
  -> Module -- ^ module dep we want info for
  -> Action (Maybe (HieFile, FilePath)) -- ^ hie stuff for the module
getHieFile file mod = do
  TransitiveDependencies {transitiveNamedModuleDeps} <- use_ GetDependencies file
  case find (\x -> nmdModuleName x == moduleName mod) transitiveNamedModuleDeps of
    Just NamedModuleDep{nmdFilePath=nfp} -> do
        let modPath = fromNormalizedFilePath nfp
        (_diags, hieFile) <- getHomeHieFile nfp
        return $ (, modPath) <$> hieFile
    _ -> getPackageHieFile mod file


getHomeHieFile :: NormalizedFilePath -> Action ([a], Maybe HieFile)
getHomeHieFile f = do
  pm <- use_ GetParsedModule f
  let normal_hie_f = toNormalizedFilePath hie_f
      hie_f = ml_hie_file $ ms_location $ pm_mod_summary pm
  mbHieTimestamp <- use GetModificationTime normal_hie_f
  srcTimestamp   <- use_ GetModificationTime f

  let isUpToDate
        | Just d <- mbHieTimestamp = comparing modificationTime d srcTimestamp == GT
        | otherwise = False

-- In the future, TypeCheck will emit .hie files as a side effect
--   unless isUpToDate $
--       void $ use_ TypeCheck f

  hf <- liftIO $ if isUpToDate then Just <$> loadHieFile hie_f else pure Nothing
  return ([], hf)

getPackageHieFile :: Module             -- ^ Package Module to load .hie file for
                  -> NormalizedFilePath -- ^ Path of home module importing the package module
                  -> Action (Maybe (HieFile, FilePath))
getPackageHieFile mod file = do
    pkgState  <- hscEnv <$> use_ GhcSession file
    IdeOptions {..} <- getIdeOptions
    let unitId = moduleUnitId mod
    case lookupPackageConfig unitId pkgState of
        Just pkgConfig -> do
            -- 'optLocateHieFile' returns Nothing if the file does not exist
            hieFile <- liftIO $ optLocateHieFile optPkgLocationOpts pkgConfig mod
            path    <- liftIO $ optLocateSrcFile optPkgLocationOpts pkgConfig mod
            case (hieFile, path) of
                (Just hiePath, Just modPath) ->
                    -- deliberately loaded outside the Shake graph
                    -- to avoid dependencies on non-workspace files
                        liftIO $ Just . (, modPath) <$> loadHieFile hiePath
                _ -> return Nothing
        _ -> return Nothing

-- | Parse the contents of a daml file.
getParsedModule :: NormalizedFilePath -> Action (Maybe ParsedModule)
getParsedModule file = use GetParsedModule file

------------------------------------------------------------
-- Rules
-- These typically go from key to value and are oracles.

priorityTypeCheck :: Priority
priorityTypeCheck = Priority 0

priorityGenerateCore :: Priority
priorityGenerateCore = Priority (-1)

priorityFilesOfInterest :: Priority
priorityFilesOfInterest = Priority (-2)

getParsedModuleRule :: Rules ()
getParsedModuleRule =
    defineEarlyCutoff $ \GetParsedModule file -> do
        (_, contents) <- getFileContents file
        packageState <- hscEnv <$> use_ GhcSession file
        opt <- getIdeOptions
        (diag, res) <- liftIO $ parseModule opt packageState (fromNormalizedFilePath file) (fmap textToStringBuffer contents)
        case res of
            Nothing -> pure (Nothing, (diag, Nothing))
            Just (contents, modu) -> do
                mbFingerprint <- if isNothing $ optShakeFiles opt
                    then pure Nothing
                    else liftIO $ Just . fingerprintToBS <$> fingerprintFromStringBuffer contents
                pure (mbFingerprint, (diag, Just modu))

getLocatedImportsRule :: Rules ()
getLocatedImportsRule =
    define $ \GetLocatedImports file -> do
        pm <- use_ GetParsedModule file
        let ms = pm_mod_summary pm
        let imports = [(False, imp) | imp <- ms_textual_imps ms] ++ [(True, imp) | imp <- ms_srcimps ms]
        env <- hscEnv <$> use_ GhcSession file
        let dflags = addRelativeImport file pm $ hsc_dflags env
        opt <- getIdeOptions
        (diags, imports') <- fmap unzip $ forM imports $ \(isSource, (mbPkgName, modName)) -> do
            diagOrImp <- locateModule dflags (optExtensions opt) getFileExists modName mbPkgName isSource
            case diagOrImp of
                Left diags -> pure (diags, Left (modName, Nothing))
                Right (FileImport path) -> pure ([], Left (modName, Just path))
                Right (PackageImport pkgId) -> liftIO $ do
                    diagsOrPkgDeps <- computePackageDeps env pkgId
                    case diagsOrPkgDeps of
                        Left diags -> pure (diags, Right Nothing)
                        Right pkgIds -> pure ([], Right $ Just $ pkgId : pkgIds)
        let (moduleImports, pkgImports) = partitionEithers imports'
        case sequence pkgImports of
            Nothing -> pure (concat diags, Nothing)
            Just pkgImports -> pure (concat diags, Just (moduleImports, Set.fromList $ concat pkgImports))


-- | Given a target file path, construct the raw dependency results by following
-- imports recursively.
rawDependencyInformation :: NormalizedFilePath -> Action RawDependencyInformation
rawDependencyInformation f = do
    let initialArtifact = ArtifactsLocation f (ModLocation (Just $ fromNormalizedFilePath f) "" "") False
        (initialId, initialMap) = getPathId initialArtifact emptyPathIdMap
    (rdi, ss) <- go (IntSet.singleton $ getFilePathId initialId)
                    ((RawDependencyInformation IntMap.empty initialMap IntMap.empty), IntMap.empty)
    let bm = IntMap.foldrWithKey (updateBootMap rdi) IntMap.empty ss
    return (rdi { rawBootMap = bm })
  where
    go fs (rawDepInfo, ss) =
        case IntSet.minView fs of
            -- Queue is empty
            Nothing -> pure (rawDepInfo, ss)
            -- Pop f from the queue and process it
            Just (f, fs) -> do
                let fId = FilePathId f
                importsOrErr <- use GetLocatedImports $ idToPath (rawPathIdMap rawDepInfo) fId
                case importsOrErr of
                  Nothing ->
                    -- File doesn’t parse
                    let rawDepInfo' = insertImport fId (Left ModuleParseError) rawDepInfo
                    in go fs (rawDepInfo', ss)
                  Just (modImports, pkgImports) -> do
                    let f :: (PathIdMap, IntMap ArtifactsLocation)
                          -> (a, Maybe ArtifactsLocation)
                          -> ((PathIdMap, IntMap ArtifactsLocation), (a, Maybe FilePathId))
                        f (pathMap, ss) (imp, mbPath) = case mbPath of
                            Nothing -> ((pathMap, ss), (imp, Nothing))
                            Just path ->
                                let (pathId, pathMap') = getPathId path pathMap
                                    ss' = if isBootLocation path
                                            then IntMap.insert (getFilePathId pathId) path ss
                                            else ss
                                in ((pathMap', ss'), (imp, Just pathId))
                    -- Convert paths in imports to ids and update the path map
                    let ((pathIdMap, ss'), modImports') = mapAccumL f (rawPathIdMap rawDepInfo, ss) modImports
                    -- Files that we haven’t seen before are added to the queue.
                    let newFiles =
                            IntSet.fromList (coerce $ mapMaybe snd modImports')
                            IntSet.\\ IntMap.keysSet (rawImports rawDepInfo)
                    let rawDepInfo' = insertImport fId (Right $ ModuleImports modImports' pkgImports) rawDepInfo
                    go (newFiles `IntSet.union` fs)
                       (rawDepInfo' { rawPathIdMap = pathIdMap }, ss')



    updateBootMap pm boot_mod_id ArtifactsLocation{..} bm =
      if not artifactIsSource
        then
          let msource_mod_id = lookupPathToId (rawPathIdMap pm) (toNormalizedFilePath $ dropBootSuffix artifactModLocation)
          in case msource_mod_id of
               Just source_mod_id -> insertBootId source_mod_id (FilePathId boot_mod_id) bm
               Nothing -> bm
        else bm

    dropBootSuffix :: ModLocation -> FilePath
    dropBootSuffix (ModLocation (Just hs_src) _ _) = reverse . drop (length @[] "-boot") . reverse $ hs_src
    dropBootSuffix _ = error "dropBootSuffix"

getDependencyInformationRule :: Rules ()
getDependencyInformationRule =
    define $ \GetDependencyInformation file -> do
       rawDepInfo <- rawDependencyInformation file
       pure ([], Just $ processDependencyInformation rawDepInfo)

reportImportCyclesRule :: Rules ()
reportImportCyclesRule =
    define $ \ReportImportCycles file -> fmap (\errs -> if null errs then ([], Just ()) else (errs, Nothing)) $ do
        DependencyInformation{..} <- use_ GetDependencyInformation file
        let fileId = pathToId depPathIdMap file
        case IntMap.lookup (getFilePathId fileId) depErrorNodes of
            Nothing -> pure []
            Just errs -> do
                let cycles = mapMaybe (cycleErrorInFile fileId) (toList errs)
                -- Convert cycles of files into cycles of module names
                forM cycles $ \(imp, files) -> do
                    modNames <- forM files $ \fileId -> do
                        let file = idToPath depPathIdMap fileId
                        getModuleName file
                    pure $ toDiag imp $ sort modNames
    where cycleErrorInFile f (PartOfCycle imp fs)
            | f `elem` fs = Just (imp, fs)
          cycleErrorInFile _ _ = Nothing
          toDiag imp mods = (fp , ShowDiag , ) $ Diagnostic
            { _range = (_range :: Location -> Range) loc
            , _severity = Just DsError
            , _source = Just "Import cycle detection"
            , _message = "Cyclic module dependency between " <> showCycle mods
            , _code = Nothing
            , _relatedInformation = Nothing
            }
            where loc = srcSpanToLocation (getLoc imp)
                  fp = toNormalizedFilePath $ srcSpanToFilename (getLoc imp)
          getModuleName file = do
           pm <- use_ GetParsedModule file
           pure (moduleNameString . moduleName . ms_mod $ pm_mod_summary pm)
          showCycle mods  = T.intercalate ", " (map T.pack mods)

-- returns all transitive dependencies in topological order.
-- NOTE: result does not include the argument file.
getDependenciesRule :: Rules ()
getDependenciesRule =
    defineEarlyCutoff $ \GetDependencies file -> do
        depInfo <- use_ GetDependencyInformation file
        let allFiles = reachableModules depInfo
        _ <- uses_ ReportImportCycles allFiles
        opts <- getIdeOptions
        let mbFingerprints = map (fingerprintString . fromNormalizedFilePath) allFiles <$ optShakeFiles opts
        return (fingerprintToBS . fingerprintFingerprints <$> mbFingerprints, ([], transitiveDeps depInfo file))

-- Source SpanInfo is used by AtPoint and Goto Definition.
getSpanInfoRule :: Rules ()
getSpanInfoRule =
    define $ \GetSpanInfo file -> do
        tc <- use_ TypeCheck file
        deps <- maybe (TransitiveDependencies [] [] []) fst <$> useWithStale GetDependencies file
        let tdeps = transitiveModuleDeps deps
        parsedDeps <- uses_ GetParsedModule tdeps
        ifaces <- uses_ GetModIface tdeps
        (fileImports, _) <- use_ GetLocatedImports file
        packageState <- hscEnv <$> use_ GhcSession file
        let imports = second (fmap artifactFilePath) <$> fileImports
        x <- liftIO $ getSrcSpanInfos packageState imports tc (zip parsedDeps $ map hirModIface ifaces)
        return ([], Just x)

-- Typechecks a module.
typeCheckRule :: Rules ()
typeCheckRule = define $ \TypeCheck file -> do
  pm   <- use_ GetParsedModule file
  deps <- use_ GetDependencies file
  hsc  <- hscEnv <$> use_ GhcSession file
  logger <- actionLogger
  -- Figure out whether we need TemplateHaskell or QuasiQuotes support
  let graph_needs_th_qq = needsTemplateHaskellOrQQ $ hsc_mod_graph hsc
      file_uses_th_qq   = uses_th_qq $ ms_hspp_opts (pm_mod_summary pm)
      any_uses_th_qq    = graph_needs_th_qq || file_uses_th_qq
  mirs      <- uses_ GetModIface (transitiveModuleDeps deps)
  bytecodes <- if any_uses_th_qq
    then -- If we use TH or QQ, we must obtain the bytecode
      fmap Just <$> uses_ GenerateByteCode (transitiveModuleDeps deps)
    else
      pure $ repeat Nothing

  setPriority priorityTypeCheck
  IdeOptions { optDefer = defer } <- getIdeOptions

  res <- liftIO $ typecheckModule defer hsc (zipWith unpack mirs bytecodes) pm

  when supportsHieFiles $
    whenJust (snd res) $ \(hsc, tcm) -> do
      (_, contents) <- getFileContents file
      liftIO $ generateAndWriteHieFile hsc (TE.encodeUtf8 <$> contents) (tmrModule tcm)
        `catch` \(e::IOException) -> do
          logDebug logger $ T.pack $ "Error saving .hie file: " <> show e
      liftIO $ generateAndWriteHiFile hsc tcm
        `catch` \(e::IOException) -> do
          logDebug logger $ T.pack $ "Error saving .hi file: " <> show e

  return $ second (fmap snd) res
 where
  unpack HiFileResult{..} bc = (hirModSummary, (hirModIface, bc))
  uses_th_qq dflags =
    xopt LangExt.TemplateHaskell dflags || xopt LangExt.QuasiQuotes dflags


generateCore :: NormalizedFilePath -> Action (IdeResult (SafeHaskellMode, CgGuts, ModDetails))
generateCore file = do
    deps <- use_ GetDependencies file
    (tm:tms) <- uses_ TypeCheck (file:transitiveModuleDeps deps)
    setPriority priorityGenerateCore
    packageState <- hscEnv <$> use_ GhcSession file
    liftIO $ compileModule packageState [(tmrModSummary x, tmrModInfo x) | x <- tms] tm

generateCoreRule :: Rules ()
generateCoreRule =
    define $ \GenerateCore -> generateCore

generateByteCodeRule :: Rules ()
generateByteCodeRule =
    define $ \GenerateByteCode file -> do
      deps <- use_ GetDependencies file
      (tm : tms) <- uses_ TypeCheck (file: transitiveModuleDeps deps)
      session <- hscEnv <$> use_ GhcSession file
      (_, guts, _) <- use_ GenerateCore file
      liftIO $ generateByteCode session [(tmrModSummary x, tmrModInfo x) | x <- tms] tm guts

-- A local rule type to get caching. We want to use newCache, but it has
-- thread killed exception issues, so we lift it to a full rule.
-- https://github.com/digital-asset/daml/pull/2808#issuecomment-529639547
type instance RuleResult GhcSessionIO = GhcSessionFun

data GhcSessionIO = GhcSessionIO deriving (Eq, Show, Typeable, Generic)
instance Hashable GhcSessionIO
instance NFData   GhcSessionIO
instance Binary   GhcSessionIO

newtype GhcSessionFun = GhcSessionFun (FilePath -> Action HscEnvEq)
instance Show GhcSessionFun where show _ = "GhcSessionFun"
instance NFData GhcSessionFun where rnf !_ = ()


loadGhcSession :: Rules ()
loadGhcSession = do
    defineNoFile $ \GhcSessionIO -> do
        opts <- getIdeOptions
        GhcSessionFun <$> optGhcSession opts
    defineEarlyCutoff $ \GhcSession file -> do
        GhcSessionFun fun <- useNoFile_ GhcSessionIO
        val <- fun $ fromNormalizedFilePath file
        opts <- getIdeOptions
        return ("" <$ optShakeFiles opts, ([], Just val))

getHiFileRule :: Rules ()
getHiFileRule = defineEarlyCutoff $ \GetHiFile f -> do
  session <- hscEnv <$> use_ GhcSession f
  logger  <- actionLogger
  -- get all dependencies interface files, to check for freshness
  (deps,_)<- use_ GetLocatedImports f
  depHis  <- traverse (use GetHiFile) (mapMaybe (fmap artifactFilePath . snd) deps)

  -- TODO find the hi file without relying on the parsed module
  --      it should be possible to construct a ModSummary parsing just the imports
  --      (see HeaderInfo in the GHC package)
  pm      <- use_ GetParsedModule f
  let hiFile = case ms_hsc_src ms of
                HsBootFile -> addBootSuffix (ml_hi_file $ ms_location ms)
                _ -> ml_hi_file $ ms_location ms
      ms     = pm_mod_summary pm

  case sequence depHis of
    Nothing -> do
          liftIO $ logDebug logger $ T.pack ("Missing dependencies for interface file: " <> hiFile)
          pure (Nothing, ([], Nothing))
    Just deps -> do
      gotHiFile <- getFileExists $ toNormalizedFilePath hiFile
      if not gotHiFile
        then do
          liftIO $ logDebug logger $ T.pack ("Missing interface file: " <> hiFile)
          pure (Nothing, ([], Nothing))
        else do
          hiVersion  <- use_ GetModificationTime $ toNormalizedFilePath hiFile
          modVersion <- use_ GetModificationTime f
          let sourceModified = LT == comparing modificationTime hiVersion modVersion
          if sourceModified
            then do
              liftIO $ logDebug logger $ T.pack ("Stale interface file: " <> hiFile)
              pure (Nothing, ([], Nothing))
            else do
              r <- liftIO $ loadInterface session ms deps
              case r of
                Right iface -> do
                  let result = HiFileResult ms iface
                  return (Just (fingerprintToBS (mi_mod_hash iface)), ([], Just result))
                Left err -> do
                  let d = ideErrorText f errMsg
                      errMsg = T.pack err
                  liftIO
                    $  logDebug logger
                    $  T.pack ("Failed to load interface file " <> hiFile <> ": ")
                    <> errMsg
                  return (Nothing, ([d], Nothing))

getModIfaceRule :: Rules ()
getModIfaceRule = define $ \GetModIface f -> do
    fileOfInterest <- use_ IsFileOfInterest f
    let useHiFile =
          -- Interface files do not carry location information, so
          -- never use interface files if .hie files are not available
          supportsHieFiles &&
          -- Never load interface files for files of interest
          not fileOfInterest
    mbHiFile <- if useHiFile then use GetHiFile f else return Nothing
    case mbHiFile of
        Just x ->
            return ([], Just x)
        Nothing -> do
            tmr <- use_ TypeCheck f
            let iface = hm_iface (tmrModInfo tmr)
            return ([], Just $ HiFileResult (tmrModSummary tmr) iface)

isFileOfInterestRule :: Rules ()
isFileOfInterestRule = defineEarlyCutoff $ \IsFileOfInterest f -> do
    filesOfInterest <- getFilesOfInterest
    let res = f `elem` filesOfInterest
    return (Just (if res then "1" else ""), ([], Just res))

-- | A rule that wires per-file rules together
mainRule :: Rules ()
mainRule = do
    getParsedModuleRule
    getLocatedImportsRule
    getDependencyInformationRule
    reportImportCyclesRule
    getDependenciesRule
    typeCheckRule
    getSpanInfoRule
    generateCoreRule
    generateByteCodeRule
    loadGhcSession
    getHiFileRule
    getModIfaceRule
    isFileOfInterestRule
