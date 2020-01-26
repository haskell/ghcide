-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main(main) where

import Arguments
import Data.Maybe
import Data.List.Extra
import Data.Void
import System.FilePath
import Control.Concurrent.Extra
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Default
import System.Time.Extra
import Development.IDE.Core.FileStore
import Development.IDE.Core.OfInterest
import Development.IDE.Core.Service
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake
import Development.IDE.Core.RuleTypes
import Development.IDE.LSP.Protocol
import Development.IDE.Types.Location
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Options
import Development.IDE.Types.Logger
import Development.IDE.GHC.Util
import Development.IDE.Plugin.Completions
import Development.IDE.Plugin.CodeAction
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types (LspId(IdInt))
import Linker
import Data.Version
import Development.IDE.LSP.LanguageServer
import System.Directory.Extra as IO
import System.Environment
import System.IO
import System.Exit
import Paths_ghcide
import Development.GitRev
import Development.Shake (Action, action)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import GhcMonad
import HscTypes (HscEnv(..), ic_dflags)
import DynFlags (parseDynamicFlagsFull, flagsPackage, flagsDynamic)
import GHC hiding (def)
import qualified GHC.Paths

import HIE.Bios

-- Set the GHC libdir to the nix libdir if it's present.
getLibdir :: IO FilePath
getLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

ghcideVersion :: IO String
ghcideVersion = do
  path <- getExecutablePath
  let gitHashSection = case $(gitHash) of
        x | x == "UNKNOWN" -> ""
        x -> " (GIT hash: " <> x <> ")"
  return $ "ghcide version: " <> showVersion version
             <> " (GHC: " <> VERSION_ghc
             <> ") (PATH: " <> path <> ")"
             <> gitHashSection

main :: IO ()
main = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    Arguments{..} <- getArguments

    if argsVersion then ghcideVersion >>= putStrLn >> exitSuccess
    else hPutStrLn stderr {- see WARNING above -} =<< ghcideVersion

    -- lock to avoid overlapping output on stdout
    lock <- newLock
    let logger p = Logger $ \pri msg -> when (pri >= p) $ withLock lock $
            T.putStrLn $ T.pack ("[" ++ upper (show pri) ++ "] ") <> msg

    whenJust argsCwd setCurrentDirectory

    dir <- getCurrentDirectory

    let handlers =
            setHandlersCompletion <>
            setHandlersCodeAction <> setHandlersCodeLens
    let rules = do
            mainRule
            produceCompletions

    if argLSP then do
        t <- offsetTime
        hPutStrLn stderr "Starting LSP server..."
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcidie WITHOUT the --lsp option!"
        runLanguageServer def handlers $ \getLspId event vfs caps -> do
            t <- t
            hPutStrLn stderr $ "Started LSP server in " ++ showDuration t
            -- very important we only call loadSession once, and it's fast, so just do it before starting
            session <- loadSession dir
            let options = (defaultIdeOptions $ return session)
                    { optReportProgress = clientSupportsProgress caps
                    , optShakeProfiling = argsShakeProfiling
                    }
            initialise caps (rules >> action kick) getLspId event (logger minBound) options vfs
    else do
        putStrLn $ "Ghcide setup tester in " ++ dir ++ "."
        putStrLn "Report bugs at https://github.com/digital-asset/ghcide/issues"

        putStrLn $ "\nStep 1/6: Finding files to test in " ++ dir
        files <- nubOrd <$> expandFiles (argFiles ++ ["." | null argFiles])
        putStrLn $ "Found " ++ show (length files) ++ " files"

        putStrLn "\nStep 2/6: Looking for hie.yaml files that control setup"
        cradles <- mapM findCradle files
        let ucradles = nubOrd cradles
        let n = length ucradles
        putStrLn $ "Found " ++ show n ++ " cradle" ++ ['s' | n /= 1]
        putStrLn "\nStep 3/6: Initializing the IDE"
        vfs <- makeVFSHandle
        grab <- loadSession dir
        ide <- initialise def mainRule (pure $ IdInt 0) (showEvent lock) (logger Info) (defaultIdeOptions $ return grab) vfs

        putStrLn "\nStep 4/6: Type checking the files"
        setFilesOfInterest ide $ Set.fromList $ map toNormalizedFilePath files
        results <- runActionSync ide $ uses TypeCheck $ map toNormalizedFilePath files
        let (worked, failed) = partition fst $ zip (map isJust results) files
        when (failed /= []) $
            putStr $ unlines $ "Files that failed:" : map ((++) " * " . snd) failed

        let files xs = let n = length xs in if n == 1 then "1 file" else show n ++ " files"
        putStrLn $ "\nCompleted (" ++ files worked ++ " worked, " ++ files failed ++ " failed)"

        unless (null failed) exitFailure


expandFiles :: [FilePath] -> IO [FilePath]
expandFiles = concatMapM $ \x -> do
    b <- IO.doesFileExist x
    if b then return [x] else do
        let recurse "." = True
            recurse x | "." `isPrefixOf` takeFileName x = False -- skip .git etc
            recurse x = takeFileName x `notElem` ["dist","dist-newstyle"] -- cabal directories
        files <- filter (\x -> takeExtension x `elem` [".hs",".lhs"]) <$> listFilesInside (return . recurse) x
        when (null files) $
            fail $ "Couldn't find any .hs/.lhs files inside directory: " ++ x
        return files


kick :: Action ()
kick = do
    files <- getFilesOfInterest
    void $ uses TypeCheck $ Set.toList files

-- | Print an LSP event.
showEvent :: Lock -> FromServerMessage -> IO ()
showEvent _ (EventFileDiagnostics _ []) = return ()
showEvent lock (EventFileDiagnostics (toNormalizedFilePath -> file) diags) =
    withLock lock $ T.putStrLn $ showDiagnosticsColored $ map (file,ShowDiag,) diags
showEvent lock e = withLock lock $ print e


cradleToSessionOpts :: Cradle Void -> FilePath -> IO ComponentOptions
cradleToSessionOpts cradle file = do
    cradleRes <- getCompilerOptions file cradle
    opts <- case cradleRes of
        CradleSuccess r -> pure r
        CradleFail err -> throwIO err
        -- TODO Rather than failing here, we should ignore any files that use this cradle.
        -- That will require some more changes.
        CradleNone -> fail "'none' cradle is not yet supported"
    pure opts

emptyHscEnv :: IO HscEnv
emptyHscEnv = do
    libdir <- getLibdir
    env <- runGhc (Just libdir) getSession
    initDynLinker env
    pure env

addPackageOpts :: HscEnv -> ComponentOptions -> IO HscEnv
addPackageOpts hscEnv opts = do
    runGhcEnv hscEnv $ do
        df <- getSessionDynFlags
        (df', _, _) <- parseDynamicFlagsFull flagsPackage True df (map noLoc $ componentOptions opts)
        _targets <- setSessionDynFlags df'
        getSession

tweakHscEnv :: HscEnv -> ComponentOptions -> IO HscEnv
tweakHscEnv hscEnv opts = do
    runGhcEnv hscEnv $ do
        df <- getSessionDynFlags
        (df', _, _) <- parseDynamicFlagsFull flagsDynamic True df (map noLoc $ componentOptions opts)
        modifySession $ \h -> h { hsc_dflags = df', hsc_IC = (hsc_IC h) { ic_dflags = df' } }
        getSession

optsToSession :: ComponentOptions -> IO HscEnvEq
optsToSession opts = do
    env <- emptyHscEnv
    env <- addPackageOpts env opts
    env <- tweakHscEnv env opts
    newHscEnvEq env

deriving instance Ord ComponentOptions

loadSession :: FilePath -> IO (FilePath -> Action HscEnvEq)
loadSession dir = do
    -- This caches the mapping from Mod.hs -> hie.yaml
    cradleLoc <- memoIO $ \v -> do
        res <- findCradle v
        -- Sometimes we get C:, sometimes we get c:, and sometimes we get a relative path
        -- try and normalise that
        -- e.g. see https://github.com/digital-asset/ghcide/issues/126
        res' <- traverse makeAbsolute res
        return $ normalise <$> res'
    -- This caches the mapping from hie.yaml + Mod.hs -> [String]
    sessionOpts <- memoIO $ \(hieYaml, file) -> do
        cradle <- maybe (loadImplicitCradle $ addTrailingPathSeparator dir) loadCradle hieYaml
        cradleToSessionOpts cradle file
    session <- memoIO optsToSession
    return $ \file -> liftIO $ do
        hieYaml <- cradleLoc file
        opts <- sessionOpts (hieYaml, file)
        session opts

-- | Memoize an IO function, with the characteristics:
--
--   * If multiple people ask for a result simultaneously, make sure you only compute it once.
--
--   * If there are exceptions, repeatedly reraise them.
--
--   * If the caller is aborted (async exception) finish computing it anyway.
memoIO :: Ord a => (a -> IO b) -> IO (a -> IO b)
memoIO op = do
    ref <- newVar Map.empty
    return $ \k -> join $ mask_ $ modifyVar ref $ \mp ->
        case Map.lookup k mp of
            Nothing -> do
                res <- onceFork $ op k
                return (Map.insert k res mp, res)
            Just res -> return (mp, res)
