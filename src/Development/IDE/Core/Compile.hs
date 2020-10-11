-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

-- | Based on https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/API.
--   Given a list of paths to find libraries, and a file to compile, produce a list of 'CoreModule' values.
module Development.IDE.Core.Compile
  ( TcModuleResult(..)
  , RunSimplifier(..)
  , compileModule
  , parseModule
  , parseHeader
  , typecheckModule
  , computePackageDeps
  , addRelativeImport
  , mkHiFileResultCompile
  , mkHiFileResultNoCompile
  , generateObjectCode
  , generateByteCode
  , generateHieAsts
  , writeHieFile
  , writeHiFile
  , getModSummaryFromImports
  , loadHieFile
  , loadInterface
  , loadDepModule
  , loadModuleHome
  , setupFinderCache
  , getDocsBatch
  , lookupName
  , modifyPLS
  ) where

import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Preprocessor
import Development.IDE.Core.Shake
import Development.IDE.GHC.Error
import Development.IDE.GHC.Warnings
import Development.IDE.Types.Diagnostics
import Development.IDE.GHC.Orphans()
import Development.IDE.GHC.Util
import qualified GHC.LanguageExtensions.Type as GHC
import Development.IDE.Types.Options
import Development.IDE.Types.Location

import Language.Haskell.LSP.Types (DiagnosticTag(..))

import LoadIface (loadModuleInterface)
import DriverPhases
import HscTypes
import DriverPipeline hiding (unP)

import qualified Parser
import           Lexer
#if MIN_GHC_API_VERSION(8,10,0)
import Control.DeepSeq (force, rnf)
#else
import Control.DeepSeq (rnf)
import ErrUtils
#endif

import           Finder
import           Development.IDE.GHC.Compat hiding (parseModule, typecheckModule, writeHieFile)
import qualified Development.IDE.GHC.Compat     as GHC
import qualified Development.IDE.GHC.Compat     as Compat
import           GhcMonad
import           GhcPlugins                     as GHC hiding (fst3, (<>))
import qualified HeaderInfo                     as Hdr
import           HscMain                        (makeSimpleDetails, hscDesugar, hscTypecheckRename, hscSimplify, hscGenHardCode, hscInteractive)
import           MkIface
import           StringBuffer                   as SB
import           TcRnMonad (finalSafeMode, TcGblEnv, tct_id, TcTyThing(AGlobal, ATcId), initTc, initIfaceLoad, tcg_th_coreplugins, tcg_binds)
import           TcIface                        (typecheckIface)
import           TidyPgm

import Control.Exception.Safe
import Control.Monad.Extra
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Bifunctor                           (first, second)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.IORef
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Map.Strict                          as Map
import           System.FilePath
import           System.Directory
import           System.IO.Extra
import Control.Exception (evaluate)
import Exception (ExceptionMonad)
import TcEnv (tcLookup)
import Data.Time (UTCTime)
import Linker (unload)
-- import GHCi (unloadObj)
import LinkerTypes
import Control.Concurrent.MVar


-- | Given a string buffer, return the string (after preprocessing) and the 'ParsedModule'.
parseModule
    :: IdeOptions
    -> HscEnv
    -> [PackageName]
    -> FilePath
    -> UTCTime
    -> Maybe SB.StringBuffer
    -> IO (IdeResult (StringBuffer, ParsedModule))
parseModule IdeOptions{..} env comp_pkgs filename modTime mbContents =
    fmap (either (, Nothing) id) $
    evalGhcEnv env $ runExceptT $ do
        (contents, dflags) <- preprocessor env filename mbContents
        (diag, modu) <- parseFileContents env optPreprocessor dflags comp_pkgs filename modTime contents
        return (diag, Just (contents, modu))


-- | Given a package identifier, what packages does it depend on
computePackageDeps
    :: HscEnv
    -> InstalledUnitId
    -> IO (Either [FileDiagnostic] [InstalledUnitId])
computePackageDeps env pkg = do
    let dflags = hsc_dflags env
    case lookupInstalledPackage dflags pkg of
        Nothing -> return $ Left [ideErrorText (toNormalizedFilePath' noFilePath) $
            T.pack $ "unknown package: " ++ show pkg]
        Just pkgInfo -> return $ Right $ depends pkgInfo

typecheckModule :: IdeDefer
                -> HscEnv
                -> ParsedModule
                -> IO (IdeResult (HscEnv, TcModuleResult))
typecheckModule (IdeDefer defer) hsc pm = do
    fmap (\(hsc, res) -> case res of Left d -> (d,Nothing); Right (d,res) -> (d,fmap (hsc,) res)) $
      runGhcEnv hsc $
      catchSrcErrors "typecheck" $ do

        let modSummary = pm_mod_summary pm
            dflags = ms_hspp_opts modSummary

        modSummary' <- initPlugins modSummary
        (warnings, tcm) <- withWarnings "typecheck" $ \tweak ->
            tcRnModule $ enableTopLevelWarnings
                       $ enableUnnecessaryAndDeprecationWarnings
                       $ demoteIfDefer pm{pm_mod_summary = tweak modSummary'}
        let errorPipeline = unDefer . hideDiag dflags . tagDiag
            diags = map errorPipeline warnings
            deferedError = any fst diags
        return (map snd diags, Just $ tcm{tmrDeferedError = deferedError})
    where
        demoteIfDefer = if defer then demoteTypeErrorsToWarnings else id

modifyPLS :: DynLinker -> (PersistentLinkerState -> IO (PersistentLinkerState, a)) -> IO a
modifyPLS dl f =
  modifyMVar (dl_mpls dl) (fmapFst pure . f . fromMaybe undefined)
  where fmapFst f = fmap (\(x, y) -> (f x, y))

tcRnModule :: GhcMonad m => ParsedModule -> m TcModuleResult
tcRnModule pmod = do
  let ms = pm_mod_summary pmod
  hsc_env <- getSession
  let hsc_env_tmp = hsc_env { hsc_dflags = ms_hspp_opts ms }
  liftIO $ unload hsc_env_tmp []
  (tc_gbl_env, mrn_info)
        <- liftIO $ hscTypecheckRename hsc_env_tmp ms $
                       HsParsedModule { hpm_module = parsedSource pmod,
                                        hpm_src_files = pm_extra_src_files pmod,
                                        hpm_annotations = pm_annotations pmod }
  let rn_info = case mrn_info of
        Just x -> x
        Nothing -> error "no renamed info tcRnModule"
  pure (TcModuleResult pmod rn_info tc_gbl_env False)

mkHiFileResultNoCompile :: HscEnv -> TcModuleResult -> IO HiFileResult
mkHiFileResultNoCompile session tcm = do
  let hsc_env_tmp = session { hsc_dflags = ms_hspp_opts ms }
      ms = pm_mod_summary $ tmrParsed tcm
      tcGblEnv = tmrTypechecked tcm
  details <- makeSimpleDetails hsc_env_tmp tcGblEnv
  sf <- finalSafeMode (ms_hspp_opts ms) tcGblEnv
#if MIN_GHC_API_VERSION(8,10,0)
  iface <- mkIfaceTc session sf details tcGblEnv
#else
  (iface, _) <- mkIfaceTc session Nothing sf details tcGblEnv
#endif
  let mod_info = HomeModInfo iface details Nothing
  pure $! HiFileResult ms mod_info

mkHiFileResultCompile
    :: HscEnv
    -> TcModuleResult
    -> ModGuts
    -> IO (IdeResult HiFileResult)
mkHiFileResultCompile session' tcm simplified_guts = catchErrs $ do
  let session = session' { hsc_dflags = ms_hspp_opts ms }
      ms = pm_mod_summary $ tmrParsed tcm
  -- give variables unique OccNames
  (guts, details) <- tidyProgram session simplified_guts

  (diags, obj_res) <- generateObjectCode session ms guts
  case obj_res of
    Nothing -> do
      pure (diags, Nothing)
    Just linkable -> do
#if MIN_GHC_API_VERSION(8,10,0)
      let !partial_iface = force (mkPartialIface session details simplified_guts)
      final_iface <- mkFullIface session partial_iface
#else
      (final_iface,_) <- mkIface session Nothing details simplified_guts
#endif
      let mod_info = HomeModInfo final_iface details (Just linkable)
      pure (diags, Just $! HiFileResult ms mod_info)
  where
    dflags = hsc_dflags session'
    source = "compile"
    catchErrs x = x `catches`
      [ Handler $ return . (,Nothing) . diagFromGhcException source dflags
      , Handler $ return . (,Nothing) . diagFromString source DsError (noSpan "<internal>")
      . (("Error during " ++ T.unpack source) ++) . show @SomeException
      ]

initPlugins :: GhcMonad m => ModSummary -> m ModSummary
initPlugins modSummary = do
    session <- getSession
    let df0 = flip gopt_unset Opt_ExternalInterpreter $ ms_hspp_opts modSummary
    dflags <- liftIO $ initializePlugins session df0{ ways = filter (WayDyn /=) (ways df0) }
    return modSummary{ms_hspp_opts = flip gopt_set Opt_ExternalInterpreter $ addWay' WayDyn dflags}

-- | Whether we should run the -O0 simplifier when generating core.
--
-- This is required for template Haskell to work but we disable this in DAML.
-- See #256
newtype RunSimplifier = RunSimplifier Bool

-- | Compile a single type-checked module to a 'CoreModule' value, or
-- provide errors.
compileModule
    :: RunSimplifier
    -> HscEnv
    -> ModSummary
    -> TcGblEnv
    -> IO (IdeResult ModGuts)
compileModule (RunSimplifier simplify) packageState ms tcg =
    fmap (either (, Nothing) (second Just)) $
    evalGhcEnv packageState $
        catchSrcErrors "compile" $ do
            session <- getSession
            (warnings,desugar) <- withWarnings "compile" $ \tweak -> do
               let ms' = tweak ms
               liftIO $ hscDesugar session{ hsc_dflags = ms_hspp_opts ms'} ms' tcg
            desugared_guts <-
                if simplify
                    then do
                        plugins <- liftIO $ readIORef (tcg_th_coreplugins tcg)
                        liftIO $ hscSimplify session plugins desugar
                    else pure desugar
            return (map snd warnings, desugared_guts)

generateObjectCode :: HscEnv -> ModSummary -> CgGuts -> IO (IdeResult Linkable)
generateObjectCode hscEnv summary guts = do
    fmap (either (, Nothing) (second Just)) $
        evalGhcEnv hscEnv $
          catchSrcErrors "object" $ do
              session <- getSession
              let dot_o =  ml_obj_file (ms_location summary)
              let session' = session { hsc_dflags = (hsc_dflags session) { outputFile = Just dot_o }}
                  fp = replaceExtension dot_o "s"
              liftIO $ createDirectoryIfMissing True (takeDirectory fp)
              liftIO $ do
                exists <- doesFileExist dot_o
                when exists $ removeFile dot_o
              (warnings, dot_o_fp) <-
                withWarnings "object" $ \_tweak -> liftIO $ do
                      (outputFilename, _mStub, _foreign_files) <- hscGenHardCode session' guts
#if MIN_GHC_API_VERSION(8,10,0)
                                (ms_location summary)
#else
                                (_tweak summary)
#endif
                                fp
                      compileFile session' StopLn (outputFilename, Just (As False))
              let unlinked = DotO dot_o_fp
              -- Need time to be the modification time for recompilation checking
              t <- liftIO $ getModificationTime dot_o_fp
              let linkable = LM t (ms_mod summary) [unlinked]
              pure (map snd warnings, linkable)

generateByteCode :: HscEnv -> ModSummary -> CgGuts -> IO (IdeResult Linkable)
generateByteCode hscEnv summary guts = do
    fmap (either (, Nothing) (second Just)) $
        evalGhcEnv hscEnv $
          catchSrcErrors "bytecode" $ do
              session <- getSession
              (warnings, (_, bytecode, sptEntries)) <-
                withWarnings "bytecode" $ \_tweak -> liftIO $
                      hscInteractive session guts
#if MIN_GHC_API_VERSION(8,10,0)
                                (ms_location summary)
#else
                                (_tweak summary)
#endif
              let unlinked = BCOs bytecode sptEntries
              let linkable = LM (ms_hs_date summary) (ms_mod summary) [unlinked]
              pure (map snd warnings, linkable)

demoteTypeErrorsToWarnings :: ParsedModule -> ParsedModule
demoteTypeErrorsToWarnings =
  (update_pm_mod_summary . update_hspp_opts) demoteTEsToWarns where

  demoteTEsToWarns :: DynFlags -> DynFlags
  -- convert the errors into warnings, and also check the warnings are enabled
  demoteTEsToWarns = (`wopt_set` Opt_WarnDeferredTypeErrors)
                   . (`wopt_set` Opt_WarnTypedHoles)
                   . (`wopt_set` Opt_WarnDeferredOutOfScopeVariables)
                   . (`gopt_set` Opt_DeferTypeErrors)
                   . (`gopt_set` Opt_DeferTypedHoles)
                   . (`gopt_set` Opt_DeferOutOfScopeVariables)

enableTopLevelWarnings :: ParsedModule -> ParsedModule
enableTopLevelWarnings =
  (update_pm_mod_summary . update_hspp_opts)
  ((`wopt_set` Opt_WarnMissingPatternSynonymSignatures) .
   (`wopt_set` Opt_WarnMissingSignatures))
  -- the line below would show also warnings for let bindings without signature
  -- ((`wopt_set` Opt_WarnMissingSignatures) . (`wopt_set` Opt_WarnMissingLocalSignatures)))

update_hspp_opts :: (DynFlags -> DynFlags) -> ModSummary -> ModSummary
update_hspp_opts up ms = ms{ms_hspp_opts = up $ ms_hspp_opts ms}

update_pm_mod_summary :: (ModSummary -> ModSummary) -> ParsedModule -> ParsedModule
update_pm_mod_summary up pm =
  pm{pm_mod_summary = up $ pm_mod_summary pm}

unDefer :: (WarnReason, FileDiagnostic) -> (Bool, FileDiagnostic)
unDefer (Reason Opt_WarnDeferredTypeErrors         , fd) = (True, upgradeWarningToError fd)
unDefer (Reason Opt_WarnTypedHoles                 , fd) = (True, upgradeWarningToError fd)
unDefer (Reason Opt_WarnDeferredOutOfScopeVariables, fd) = (True, upgradeWarningToError fd)
unDefer ( _                                        , fd) = (False, fd)

upgradeWarningToError :: FileDiagnostic -> FileDiagnostic
upgradeWarningToError (nfp, sh, fd) =
  (nfp, sh, fd{_severity = Just DsError, _message = warn2err $ _message fd}) where
  warn2err :: T.Text -> T.Text
  warn2err = T.intercalate ": error:" . T.splitOn ": warning:"

hideDiag :: DynFlags -> (WarnReason, FileDiagnostic) -> (WarnReason, FileDiagnostic)
hideDiag originalFlags (Reason warning, (nfp, sh, fd))
  | not (wopt warning originalFlags)
  = if null (_tags fd)
       then (Reason warning, (nfp, HideDiag, fd))
            -- keep the diagnostic if it has an associated tag
       else (Reason warning, (nfp, sh, fd{_severity = Just DsInfo}))
hideDiag _originalFlags t = t

enableUnnecessaryAndDeprecationWarnings :: ParsedModule -> ParsedModule
enableUnnecessaryAndDeprecationWarnings =
  (update_pm_mod_summary . update_hspp_opts)
  (foldr (.) id [(`wopt_set` flag) | flag <- unnecessaryDeprecationWarningFlags])

-- | Warnings which lead to a diagnostic tag
unnecessaryDeprecationWarningFlags :: [WarningFlag]
unnecessaryDeprecationWarningFlags
  = [ Opt_WarnUnusedTopBinds
    , Opt_WarnUnusedLocalBinds
    , Opt_WarnUnusedPatternBinds
    , Opt_WarnUnusedImports
    , Opt_WarnUnusedMatches
    , Opt_WarnUnusedTypePatterns
    , Opt_WarnUnusedForalls
#if MIN_GHC_API_VERSION(8,10,0)
    , Opt_WarnUnusedRecordWildcards
#endif
    , Opt_WarnInaccessibleCode
    , Opt_WarnWarningsDeprecations
    ]

-- | Add a unnecessary/deprecated tag to the required diagnostics.
tagDiag :: (WarnReason, FileDiagnostic) -> (WarnReason, FileDiagnostic)
tagDiag (Reason warning, (nfp, sh, fd))
  | Just tag <- requiresTag warning
  = (Reason warning, (nfp, sh, fd { _tags = addTag tag (_tags fd) }))
  where
    requiresTag :: WarningFlag -> Maybe DiagnosticTag
    requiresTag Opt_WarnWarningsDeprecations
      = Just DtDeprecated
    requiresTag wflag  -- deprecation was already considered above
      | wflag `elem` unnecessaryDeprecationWarningFlags
      = Just DtUnnecessary
    requiresTag _ = Nothing
    addTag :: DiagnosticTag -> Maybe (List DiagnosticTag) -> Maybe (List DiagnosticTag)
    addTag t Nothing          = Just (List [t])
    addTag t (Just (List ts)) = Just (List (t : ts))
-- other diagnostics are left unaffected
tagDiag t = t

addRelativeImport :: NormalizedFilePath -> ModuleName -> DynFlags -> DynFlags
addRelativeImport fp modu dflags = dflags
    {importPaths = nubOrd $ maybeToList (moduleImportPath fp modu) ++ importPaths dflags}

atomicFileWrite :: FilePath -> (FilePath -> IO a) -> IO ()
atomicFileWrite targetPath write = do
  let dir = takeDirectory targetPath
  createDirectoryIfMissing True dir
  (tempFilePath, cleanUp) <- newTempFileWithin dir
  (write tempFilePath >> renameFile tempFilePath targetPath) `onException` cleanUp

generateHieAsts :: HscEnv -> TcModuleResult -> IO ([FileDiagnostic], Maybe (HieASTs Type))
generateHieAsts hscEnv tcm =
  handleGenerationErrors' dflags "extended interface generation" $ runHsc hscEnv $
    Just <$> GHC.enrichHie (tcg_binds $ tmrTypechecked tcm) (tmrRenamed tcm)
  where
    dflags = hsc_dflags hscEnv

writeHieFile :: HscEnv -> ModSummary -> [GHC.AvailInfo] -> HieASTs Type -> BS.ByteString -> IO [FileDiagnostic]
writeHieFile hscEnv mod_summary exports ast source =
  handleGenerationErrors dflags "extended interface write/compression" $ do
    hf <- runHsc hscEnv $
      GHC.mkHieFile' mod_summary exports ast source
    atomicFileWrite targetPath $ flip GHC.writeHieFile hf
  where
    dflags       = hsc_dflags hscEnv
    mod_location = ms_location mod_summary
    targetPath   = Compat.ml_hie_file mod_location

writeHiFile :: HscEnv -> HiFileResult -> IO [FileDiagnostic]
writeHiFile hscEnv tc =
  handleGenerationErrors dflags "interface generation" $ do
    atomicFileWrite targetPath $ \fp ->
      writeIfaceFile dflags fp modIface
  where
    modIface = hm_iface $ hirHomeMod tc
    targetPath = ml_hi_file $ ms_location $ hirModSummary tc
    dflags = hsc_dflags hscEnv

handleGenerationErrors :: DynFlags -> T.Text -> IO () -> IO [FileDiagnostic]
handleGenerationErrors dflags source action =
  action >> return [] `catches`
    [ Handler $ return . diagFromGhcException source dflags
    , Handler $ return . diagFromString source DsError (noSpan "<internal>")
    . (("Error during " ++ T.unpack source) ++) . show @SomeException
    ]

handleGenerationErrors' :: DynFlags -> T.Text -> IO (Maybe a) -> IO ([FileDiagnostic], Maybe a)
handleGenerationErrors' dflags source action =
  fmap ([],) action `catches`
    [ Handler $ return . (,Nothing) . diagFromGhcException source dflags
    , Handler $ return . (,Nothing) . diagFromString source DsError (noSpan "<internal>")
    . (("Error during " ++ T.unpack source) ++) . show @SomeException
    ]

-- | Initialise the finder cache, dependencies should be topologically
-- sorted.
setupFinderCache :: GhcMonad m => [ModSummary] -> m ()
setupFinderCache mss = do
    session <- getSession

    -- set the target and module graph in the session
    let graph = mkModuleGraph mss
    setSession session { hsc_mod_graph = graph }

    -- Make modules available for others that import them,
    -- by putting them in the finder cache.
    let ims  = map (InstalledModule (thisInstalledUnitId $ hsc_dflags session) . moduleName . ms_mod) mss
        ifrs = zipWith (\ms -> InstalledFound (ms_location ms)) mss ims
    -- We have to create a new IORef here instead of modifying the existing IORef as
    -- it is shared between concurrent compilations.
    prevFinderCache <- liftIO $ readIORef $ hsc_FC session
    let newFinderCache =
            foldl'
                (\fc (im, ifr) -> GHC.extendInstalledModuleEnv fc im ifr) prevFinderCache
                $ zip ims ifrs
    newFinderCacheVar <- liftIO $ newIORef $! newFinderCache
    modifySession $ \s -> s { hsc_FC = newFinderCacheVar }


-- | Load a module, quickly. Input doesn't need to be desugared.
-- A module must be loaded before dependent modules can be typechecked.
-- This variant of loadModuleHome will *never* cause recompilation, it just
-- modifies the session.
--
-- The order modules are loaded is important when there are hs-boot files.
-- In particular you should make sure to load the .hs version of a file after the
-- .hs-boot version.
loadModuleHome
    :: HomeModInfo
    -> HscEnv
    -> HscEnv
loadModuleHome mod_info e =
    e { hsc_HPT = addToHpt (hsc_HPT e) mod_name mod_info, hsc_type_env_var = Nothing }
    where
      mod_name = moduleName $ mi_module $ hm_iface mod_info

-- | Load module interface.
loadDepModuleIO :: HomeModInfo -> HscEnv -> IO HscEnv
loadDepModuleIO mod_info hsc = do
    return $ loadModuleHome mod_info hsc

loadDepModule :: GhcMonad m => HomeModInfo -> m ()
loadDepModule mod_info = do
  e <- getSession
  e' <- liftIO $ loadDepModuleIO mod_info e
  setSession e'

-- | GhcMonad function to chase imports of a module given as a StringBuffer. Returns given module's
-- name and its imports.
getImportsParsed ::  DynFlags ->
               GHC.ParsedSource ->
               Either [FileDiagnostic] (GHC.ModuleName, [(Bool, (Maybe FastString, Located GHC.ModuleName))])
getImportsParsed dflags (L loc parsed) = do
  let modName = maybe (GHC.mkModuleName "Main") GHC.unLoc $ GHC.hsmodName parsed

  -- most of these corner cases are also present in https://hackage.haskell.org/package/ghc-8.6.1/docs/src/HeaderInfo.html#getImports
  -- but we want to avoid parsing the module twice
  let implicit_prelude = xopt GHC.ImplicitPrelude dflags
      implicit_imports = Hdr.mkPrelImports modName loc implicit_prelude $ GHC.hsmodImports parsed

  -- filter out imports that come from packages
  return (modName, [(ideclSource i, (fmap sl_fs $ ideclPkgQual i, ideclName i))
    | i <- map GHC.unLoc $ implicit_imports ++ GHC.hsmodImports parsed
    , GHC.moduleNameString (GHC.unLoc $ ideclName i) /= "GHC.Prim"
    ])

withBootSuffix :: HscSource -> ModLocation -> ModLocation
withBootSuffix HsBootFile = addBootSuffixLocnOut
withBootSuffix _ = id

-- | Produce a module summary from a StringBuffer.
getModSummaryFromBuffer
    :: GhcMonad m
    => FilePath
    -> UTCTime
    -> DynFlags
    -> GHC.ParsedSource
    -> StringBuffer
    -> ExceptT [FileDiagnostic] m ModSummary
getModSummaryFromBuffer fp modTime dflags parsed contents = do
  (modName, imports) <- liftEither $ getImportsParsed dflags parsed

  modLoc <- liftIO $ mkHomeModLocation dflags modName fp
  let InstalledUnitId unitId = thisInstalledUnitId dflags
  return $ ModSummary
    { ms_mod          = mkModule (fsToUnitId unitId) modName
    , ms_location     = withBootSuffix sourceType modLoc
    , ms_hs_date      = modTime
    , ms_textual_imps = [imp | (False, imp) <- imports]
    , ms_hspp_file    = fp
    , ms_hspp_opts    = dflags
        -- NOTE: It's /vital/ we set the 'StringBuffer' here, to give any
        -- registered GHC plugins access to the /updated/ in-memory content
        -- of a module being edited. Without this line, any plugin wishing to
        -- parse an input module and perform operations on the /current/ state
        -- of a file wouldn't work properly, as it would \"see\" a stale view of
        -- the file (i.e., the on-disk content of the latter).
    , ms_hspp_buf     = Just contents

    -- defaults:
    , ms_hsc_src      = sourceType
    , ms_obj_date     = Nothing
    , ms_iface_date   = Nothing
#if MIN_GHC_API_VERSION(8,8,0)
    , ms_hie_date     = Nothing
#endif
    , ms_srcimps      = [imp | (True, imp) <- imports]
    , ms_parsed_mod   = Nothing
    }
    where
      sourceType = if "-boot" `isSuffixOf` takeExtension fp then HsBootFile else HsSrcFile

-- | Given a buffer, env and filepath, produce a module summary by parsing only the imports.
--   Runs preprocessors as needed.
getModSummaryFromImports
  :: (HasDynFlags m, ExceptionMonad m, MonadIO m)
  => HscEnv
  -> FilePath
  -> UTCTime
  -> Maybe SB.StringBuffer
  -> ExceptT [FileDiagnostic] m ModSummary
getModSummaryFromImports env fp modTime contents = do
    (contents, dflags) <- preprocessor env fp contents
    (srcImports, textualImports, L _ moduleName) <-
        ExceptT $ liftIO $ first (diagFromErrMsgs "parser" dflags) <$> GHC.getHeaderImports dflags contents fp fp

    -- Force bits that might keep the string buffer and DynFlags alive unnecessarily
    liftIO $ evaluate $ rnf srcImports
    liftIO $ evaluate $ rnf textualImports

    modLoc <- liftIO $ mkHomeModLocation dflags moduleName fp

    let mod = mkModule (thisPackage dflags) moduleName
        sourceType = if "-boot" `isSuffixOf` takeExtension fp then HsBootFile else HsSrcFile
        summary =
            ModSummary
                { ms_mod          = mod
#if MIN_GHC_API_VERSION(8,8,0)
                , ms_hie_date     = Nothing
#endif
                , ms_hs_date      = modTime
                , ms_hsc_src      = sourceType
                -- The contents are used by the GetModSummary rule
                , ms_hspp_buf     = Just contents
                , ms_hspp_file    = fp
                , ms_hspp_opts    = dflags
                , ms_iface_date   = Nothing
                , ms_location     = withBootSuffix sourceType modLoc
                , ms_obj_date     = Nothing
                , ms_parsed_mod   = Nothing
                , ms_srcimps      = srcImports
                , ms_textual_imps = textualImports
                }
    return summary

-- | Parse only the module header
parseHeader
       :: GhcMonad m
       => DynFlags -- ^ flags to use
       -> FilePath  -- ^ the filename (for source locations)
       -> SB.StringBuffer -- ^ Haskell module source text (full Unicode is supported)
       -> ExceptT [FileDiagnostic] m ([FileDiagnostic], Located(HsModule GhcPs))
parseHeader dflags filename contents = do
   let loc  = mkRealSrcLoc (mkFastString filename) 1 1
   case unP Parser.parseHeader (mkPState dflags contents loc) of
#if MIN_GHC_API_VERSION(8,10,0)
     PFailed pst ->
        throwE $ diagFromErrMsgs "parser" dflags $ getErrorMessages pst dflags
#else
     PFailed _ locErr msgErr ->
        throwE $ diagFromErrMsg "parser" dflags $ mkPlainErrMsg dflags locErr msgErr
#endif
     POk pst rdr_module -> do
        let (warns, errs) = getMessages pst dflags
        -- Just because we got a `POk`, it doesn't mean there
        -- weren't errors! To clarify, the GHC parser
        -- distinguishes between fatal and non-fatal
        -- errors. Non-fatal errors are the sort that don't
        -- prevent parsing from continuing (that is, a parse
        -- tree can still be produced despite the error so that
        -- further errors/warnings can be collected). Fatal
        -- errors are those from which a parse tree just can't
        -- be produced.
        unless (null errs) $
            throwE $ diagFromErrMsgs "parser" dflags errs

        let warnings = diagFromErrMsgs "parser" dflags warns
        return (warnings, rdr_module)

-- | Given a buffer, flags, and file path, produce a
-- parsed module (or errors) and any parse warnings. Does not run any preprocessors
parseFileContents
       :: GhcMonad m
       => HscEnv
       -> (GHC.ParsedSource -> IdePreprocessedSource)
       -> DynFlags -- ^ flags to use
       -> [PackageName] -- ^ The package imports to ignore
       -> FilePath  -- ^ the filename (for source locations)
       -> UTCTime   -- ^ the modification timestamp
       -> SB.StringBuffer -- ^ Haskell module source text (full Unicode is supported)
       -> ExceptT [FileDiagnostic] m ([FileDiagnostic], ParsedModule)
parseFileContents env customPreprocessor dflags comp_pkgs filename modTime contents = do
   let loc  = mkRealSrcLoc (mkFastString filename) 1 1
   case unP Parser.parseModule (mkPState dflags contents loc) of
#if MIN_GHC_API_VERSION(8,10,0)
     PFailed pst -> throwE $ diagFromErrMsgs "parser" dflags $ getErrorMessages pst dflags
#else
     PFailed _ locErr msgErr ->
      throwE $ diagFromErrMsg "parser" dflags $ mkPlainErrMsg dflags locErr msgErr
#endif
     POk pst rdr_module ->
         let hpm_annotations =
               (Map.fromListWith (++) $ annotations pst,
                 Map.fromList ((noSrcSpan,comment_q pst)
                                  :annotations_comments pst))
             (warns, errs) = getMessages pst dflags
         in
           do
               -- Just because we got a `POk`, it doesn't mean there
               -- weren't errors! To clarify, the GHC parser
               -- distinguishes between fatal and non-fatal
               -- errors. Non-fatal errors are the sort that don't
               -- prevent parsing from continuing (that is, a parse
               -- tree can still be produced despite the error so that
               -- further errors/warnings can be collected). Fatal
               -- errors are those from which a parse tree just can't
               -- be produced.
               unless (null errs) $
                 throwE $ diagFromErrMsgs "parser" dflags errs

               -- Ok, we got here. It's safe to continue.
               let IdePreprocessedSource preproc_warns errs parsed = customPreprocessor rdr_module
               unless (null errs) $ throwE $ diagFromStrings "parser" DsError errs
               let parsed' = removePackageImports comp_pkgs parsed
               let preproc_warnings = diagFromStrings "parser" DsWarning preproc_warns
               ms <- getModSummaryFromBuffer filename modTime dflags parsed' contents
               parsed'' <- liftIO $ applyPluginsParsedResultAction env dflags ms hpm_annotations parsed
               let pm =
                     ParsedModule {
                         pm_mod_summary = ms
                       , pm_parsed_source = parsed''
                       , pm_extra_src_files=[] -- src imports not allowed
                       , pm_annotations = hpm_annotations
                      }
                   warnings = diagFromErrMsgs "parser" dflags warns
               pure (warnings ++ preproc_warnings, pm)

-- | After parsing the module remove all package imports referring to
-- these packages as we have already dealt with what they map to.
removePackageImports :: [PackageName] -> GHC.ParsedSource -> GHC.ParsedSource
removePackageImports pkgs (L l h@HsModule {hsmodImports} ) = L l (h { hsmodImports = imports' })
  where
    imports' = map do_one_import hsmodImports
    do_one_import (L l i@ImportDecl{ideclPkgQual}) =
      case PackageName . sl_fs <$> ideclPkgQual of
        Just pn | pn `elem` pkgs -> L l (i { ideclPkgQual = Nothing })
        _ -> L l i
    do_one_import l = l

loadHieFile :: Compat.NameCacheUpdater -> FilePath -> IO GHC.HieFile
loadHieFile ncu f = do
  GHC.hie_file_result <$> GHC.readHieFile ncu f

-- | Retuns an up-to-date module interface, regenerating if needed.
--   Assumes file exists.
--   Requires the 'HscEnv' to be set up with dependencies
loadInterface
  :: MonadIO m => HscEnv
  -> ModSummary
  -> SourceModified
  -> Bool
  -> (Bool -> m ([FileDiagnostic], Maybe HiFileResult)) -- ^ Action to regenerate an interface
  -> m ([FileDiagnostic], Maybe HiFileResult)
loadInterface session ms sourceMod objNeeded regen = do
    res <- liftIO $ checkOldIface session ms sourceMod Nothing
    case res of
          (UpToDate, Just iface)
            -- If the module used TH splices when it was last
            -- compiled, then the recompilation check is not
            -- accurate enough (https://gitlab.haskell.org/ghc/ghc/-/issues/481)
            -- and we must ignore
            -- it.  However, if the module is stable (none of
            -- the modules it depends on, directly or
            -- indirectly, changed), then we *can* skip
            -- recompilation. This is why the SourceModified
            -- type contains SourceUnmodifiedAndStable, and
            -- it's pretty important: otherwise ghc --make
            -- would always recompile TH modules, even if
            -- nothing at all has changed. Stability is just
            -- the same check that make is doing for us in
            -- one-shot mode.
            | not (mi_used_th iface) || SourceUnmodifiedAndStable == sourceMod
            -> do
             linkable <-
               if objNeeded
               then liftIO $ findObjectLinkableMaybe (ms_mod ms) (ms_location ms)
               else pure Nothing
             let objUpToDate = not objNeeded || case linkable of
                   Nothing -> False
                   Just (LM obj_time _ _) -> obj_time > ms_hs_date ms
             if objUpToDate
             then do
               hmi <- liftIO $ mkDetailsFromIface session iface linkable
               return ([], Just $ HiFileResult ms hmi)
             else regen objNeeded
          (_reason, _) -> regen objNeeded

mkDetailsFromIface :: HscEnv -> ModIface -> Maybe Linkable -> IO HomeModInfo
mkDetailsFromIface session iface linkable = do
  details <- liftIO $ fixIO $ \details -> do
    let hsc' = session { hsc_HPT = addToHpt (hsc_HPT session) (moduleName $ mi_module iface) (HomeModInfo iface details linkable) }
    initIfaceLoad hsc' (typecheckIface iface)
  return (HomeModInfo iface details linkable)

-- | Non-interactive, batch version of 'InteractiveEval.getDocs'.
--   The interactive paths create problems in ghc-lib builds
--- and leads to fun errors like "Cannot continue after interface file error".
getDocsBatch :: GhcMonad m
        => Module  -- ^ a moudle where the names are in scope
        -> [Name]
        -> m [Either String (Maybe HsDocString, Map.Map Int HsDocString)]
getDocsBatch _mod _names =
  withSession $ \hsc_env -> liftIO $ do
    ((_warns,errs), res) <- initTc hsc_env HsSrcFile False _mod fakeSpan $ forM _names $ \name ->
        case nameModule_maybe name of
            Nothing -> return (Left $ NameHasNoModule name)
            Just mod -> do
             ModIface { mi_doc_hdr = mb_doc_hdr
                      , mi_decl_docs = DeclDocMap dmap
                      , mi_arg_docs = ArgDocMap amap
                      } <- loadModuleInterface "getModuleInterface" mod
             if isNothing mb_doc_hdr && Map.null dmap && Map.null amap
               then pure (Left (NoDocsInIface mod $ compiled name))
               else pure (Right ( Map.lookup name dmap
                                , Map.findWithDefault Map.empty name amap))
    case res of
        Just x -> return $ map (first prettyPrint) x
        Nothing -> throwErrors errs
  where
    throwErrors = liftIO . throwIO . mkSrcErr
    compiled n =
      -- TODO: Find a more direct indicator.
      case nameSrcLoc n of
        RealSrcLoc {} -> False
        UnhelpfulLoc {} -> True

fakeSpan :: RealSrcSpan
fakeSpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "<ghcide>") 1 1

-- | Non-interactive, batch version of 'InteractiveEval.lookupNames'.
--   The interactive paths create problems in ghc-lib builds
--- and leads to fun errors like "Cannot continue after interface file error".
lookupName :: GhcMonad m
           => Module -- ^ A module where the Names are in scope
           -> Name
           -> m (Maybe TyThing)
lookupName mod name = withSession $ \hsc_env -> liftIO $ do
    (_messages, res) <- initTc hsc_env HsSrcFile False mod fakeSpan $ do
        tcthing <- tcLookup name
        case tcthing of
            AGlobal thing    -> return thing
            ATcId{tct_id=id} -> return (AnId id)
            _ -> panic "tcRnLookupName'"
    return res
