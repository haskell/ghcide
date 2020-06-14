{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-  Bench history

    A Shake script to analyze the performance of ghcide over the git history of the project

    Driven by a config file `bench/hist.yaml` containing the list of Git references to analyze.

    Builds each one of them and executes a set of experiments using the ghcide-bench suite.

    The results of the benchmarks and the analysis are recorded in the file
    system with the following structure:

    bench-hist
    ├── <git-reference>                       - one folder per version
    │   ├── <experiment>.benchmark-gcStats    - RTS -s output
    │   ├── <experiment>.csv                  - stats for the experiment
    │   ├── <experiment>.svg                  - Graph of bytes over elapsed time
    │   ├── <experiment>.diff.svg             - idem, including the previous version
    │   ├── <experiment>.log                  - ghcide-bench output
    │   ├── ghc.path                          - path to ghc used to build the binary
    │   ├── ghcide                            - binary for this version
    │   └── results.csv                       - results of all the experiments for the version
    ├── results.csv        - aggregated results of all the experiments and versions
    ├── <experiment>.svg   - graph of bytes over elapsed time, for all the included versions

   For diff graphs, the "previous version" is the preceding entry in the list of versions
   in the config file. A possible improvement is to obtain this info via `git rev-list`.

   The script relies on stack for building and running all the binaries.

   To execute the script:

   > stack build ghcide:exe:benchHist && stack exec benchHist all

   To build a specific analysis, enumerate the desired file artifacts

   > stack exec benchHist bench-hist/HEAD/results.csv bench-hist/HEAD/edit.diff.svg

 -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative (Alternative (empty))
import Control.Monad (forM, forM_, replicateM)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml ((.!=), (.:), (.:?), FromJSON (..), ToJSON (..), Value (..), decodeFileThrow)
import Development.Shake
import Development.Shake.Classes (Binary, Hashable, NFData)
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import qualified Graphics.Rendering.Chart.Backend.Diagrams as E
import Graphics.Rendering.Chart.Easy ((.=))
import qualified Graphics.Rendering.Chart.Easy as E
import Numeric.Natural (Natural)
import System.Directory
import System.FilePath
import qualified Text.ParserCombinators.ReadP as P
import Text.Read (Read (..), get, readMaybe, readP_to_Prec)

config :: FilePath
config = "bench/hist.yaml"

-- | Read the config without dependency
readConfigIO :: IO Config
readConfigIO = do
  decodeFileThrow config

readConfig :: Action Config
readConfig = need [config] >> liftIO readConfigIO

build :: String
build = "bench-hist"

newtype GetSamples = GetSamples () deriving newtype (Binary, Eq, Hashable, NFData, Show)

newtype GetExperiments = GetExperiments () deriving newtype (Binary, Eq, Hashable, NFData, Show)

newtype GetVersions = GetVersions () deriving newtype (Binary, Eq, Hashable, NFData, Show)

newtype GetParent = GetParent Text deriving newtype (Binary, Eq, Hashable, NFData, Show)

newtype GetCommitId = GetCommitId String deriving newtype (Binary, Eq, Hashable, NFData, Show)

type instance RuleResult GetSamples = Natural

type instance RuleResult GetExperiments = [Unescaped String]

type instance RuleResult GetVersions = [GitCommit]

type instance RuleResult GetParent = Text

type instance RuleResult GetCommitId = String

main :: IO ()
main = shakeArgs shakeOptions {shakeChange = ChangeModtimeAndDigest} $ do
  want ["all"]

  _ <- addOracle $ \GetSamples {} -> samples <$> readConfig
  _ <- addOracle $ \GetExperiments {} -> experiments <$> readConfig
  _ <- addOracle $ \GetVersions {} -> versions <$> readConfig
  _ <- addOracle $ \(GetParent name) -> findPrev name . versions <$> readConfig
  _ <- addOracle $ \(GetCommitId human) -> (`nameToGit` human) . versions <$> readConfig

  let readVersions = askOracle $ GetVersions ()
      readExperiments = askOracle $ GetExperiments ()
      readSamples = askOracle $ GetSamples ()
      getParent = askOracle . GetParent
      getCommitId = askOracle . GetCommitId

  phony "all" $ do
    Config {..} <- readConfig

    forM_ versions $ \ver ->
      need [build </> T.unpack (humanName ver) </> "results.csv"]

    need $
      [build </> "results.csv"]
        ++ [ build </> escaped (escapeExperiment e) <.> "svg"
             | e <- experiments
           ]
        ++ [ build </> T.unpack (humanName ver) </> escaped (escapeExperiment e) <.> mode <.> "svg"
             | e <- experiments,
               ver <- versions,
               mode <- ["", "diff"]
           ]

  priority 10 $ [build -/- "HEAD/ghcide"
                , build -/- "HEAD/ghc.path"
                ]
    &%> \[out, ghcpath] -> do
      liftIO $ createDirectoryIfMissing True $ dropFileName out
      need =<< getDirectoryFiles "." ["src//*.hs", "exe//*.hs", "ghcide.cabal"]
      cmd_
          ( "stack --local-bin-path=" <> takeDirectory out
              <> " --stack-yaml=stack88.yaml build ghcide:ghcide --copy-bins --ghc-options -rtsopts"
          )
      Stdout ghcLoc <- cmd (s "stack --stack-yaml=stack88.yaml exec which ghc")
      writeFile' ghcpath ghcLoc

  [ build -/- "*/ghcide",
    build -/- "*/ghc.path"
    ]
    &%> \[out, ghcpath] -> do
      let [_, ver, _] = splitDirectories out
      liftIO $ createDirectoryIfMissing True $ dropFileName out
      commitid <- getCommitId ver
      cmd_ $ "git worktree add bench-temp " ++ commitid
      flip actionFinally (cmd_ (s "git worktree remove bench-temp --force")) $ do
        Stdout ghcLoc <- cmd [Cwd "bench-temp"] (s "stack --stack-yaml=stack88.yaml exec which ghc")
        cmd_
          [Cwd "bench-temp"]
          ( "stack --local-bin-path=../"
              <> takeDirectory out
              <> " --stack-yaml=stack88.yaml build ghcide:ghcide --copy-bins --ghc-options -rtsopts"
          )
        writeFile' ghcpath ghcLoc

  priority 8000 $
    build -/- "*/results.csv" %> \out -> do
      experiments <- readExperiments

      let allResultFiles = [takeDirectory out </> escaped (escapeExperiment e) <.> "csv" | e <- experiments]
      allResults <- traverse readFileLines allResultFiles

      let header = head $ head allResults
          results = map tail allResults
      writeFileChanged out $ unlines $ header : concat results

  ghcideBenchResource <- newResource "ghcide-bench" 1

  priority 0 $
    [ build -/- "*/*.csv",
      build -/- "*/*.benchmark-gcStats"
    ]
      &%> \[outcsv, _outGc] -> do
        let [_, _, exp] = splitDirectories outcsv
        samples <- readSamples
        liftIO $ createDirectoryIfMissing True $ dropFileName outcsv
        let ghcide = dropFileName outcsv </> "ghcide"
            ghcpath = dropFileName outcsv </> "ghc.path"
        need [ghcide, ghcpath]
        ghcPath <- readFile' ghcpath
        ghcideBenchPath <- ghcideBench <$> liftIO readConfigIO
        verb <- getVerbosity
        withResource ghcideBenchResource 1 $ do
          Stdout res <-
            command
              [ EchoStdout True,
                RemEnv "NIX_GHC_LIBDIR",
                RemEnv "GHC_PACKAGE_PATH",
                AddPath [takeDirectory ghcPath, "."] []
              ]
              ghcideBenchPath
              [ "--timeout=3000",
                "--samples=" <> show samples,
                "--csv=" <> outcsv,
                "--example-package-version=3.0.0.0",
                "--rts=-I0.5",
                "--ghcide=" <> ghcide,
                "--select",
                unescaped (unescapeExperiment (Escaped $ dropExtension exp)),
                if verb > Normal then "-v" else "-q"
              ]
          writeFile' (replaceExtension outcsv "log") res
          cmd_ Shell $ "mv *.benchmark-gcStats " <> dropFileName outcsv

  build -/- "results.csv" %> \out -> do
    versions <- readVersions
    let allResultFiles =
          [build </> T.unpack (humanName v) </> "results.csv" | v <- versions]

    need [build </> T.unpack (humanName v) </> "ghcide" | v <- versions]

    allResults <- traverse readFileLines allResultFiles

    let header = head $ head allResults
        results = map tail allResults
        header' = "version, " <> header
        results' = zipWith (\v -> map (\l -> T.unpack (humanName v) <> ", " <> l)) versions results

    writeFileChanged out $ unlines $ header' : concat results'

  priority 2 $
    build -/- "*/*.diff.svg" %> \out -> do
      let [b, ver, exp_] = splitDirectories out
          exp = Escaped $ dropExtension $ dropExtension exp_
      prev <- getParent $ T.pack ver

      runLog <- loadRunLog b exp ver
      runLogPrev <- loadRunLog b exp $ T.unpack prev

      let diagram = Diagram Live [runLog, runLogPrev] title
          title = show (unescapeExperiment exp) <> " - live bytes over time compared"
      plotDiagram diagram out

  priority 1 $
    build -/- "*/*.svg" %> \out -> do
      let [b, ver, exp] = splitDirectories out
      runLog <- loadRunLog b (Escaped $ dropExtension exp) ver
      let diagram = Diagram Live [runLog] title
          title = ver <> " live bytes over time"
      plotDiagram diagram out

  build -/- "*.svg" %> \out -> do
    let exp = Escaped $ dropExtension $ takeFileName out
    versions <- readVersions

    runLogs <- forM (filter include versions) $ \v -> do
      loadRunLog build exp $ T.unpack $ humanName v

    let diagram = Diagram Live runLogs title
        title = show (unescapeExperiment exp) <> " - live bytes over time"
    plotDiagram diagram out

----------------------------------------------------------------------------------------------------

data Config = Config
  { experiments :: [Unescaped String],
    samples :: Natural,
    versions :: [GitCommit],
    -- | Path to the ghcide-bench binary for the experiments
    ghcideBench :: FilePath
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data GitCommit = GitCommit
  { -- | A git hash, tag or branch name (e.g. v0.1.0)
    gitName :: Text,
    -- | A human understandable name (e.g. fix-collisions-leak)
    name :: Maybe Text,
    -- | The human understandable name of the parent, if specified explicitly
    parent :: Maybe Text,
    -- | Whether to include this version in the top chart
    include :: Bool
  }
  deriving (Binary, Eq, Hashable, Generic, NFData, Show)

instance FromJSON GitCommit where
  parseJSON (String s) = pure $ GitCommit s Nothing Nothing True
  parseJSON (Object (toList -> [(name, String gitName)])) =
    pure $ GitCommit gitName (Just name) Nothing True
  parseJSON (Object (toList -> [(name, Object props)])) =
    GitCommit
      <$> props .: "git"
      <*> pure (Just name)
      <*> props .:? "parent"
      <*> props .:? "include" .!= True
  parseJSON _ = empty

instance ToJSON GitCommit where
  toJSON GitCommit {..} =
    case name of
      Nothing -> String gitName
      Just n -> Object $ fromList [(n, String gitName)]

humanName :: GitCommit -> Text
humanName GitCommit {..} = fromMaybe gitName name

nameToGit :: [GitCommit] -> String -> String
nameToGit versions n =
  maybe n (T.unpack . gitName) $ find ((== T.pack n) . humanName) versions

findPrev :: Text -> [GitCommit] -> Text
findPrev name (x : y : xx)
  | humanName y == name = humanName x
  | otherwise = findPrev name (y : xx)
findPrev name _ = name

----------------------------------------------------------------------------------------------------

-- | A line in the output of -S
data Frame = Frame
  { allocated, copied, live :: !Int,
    user, elapsed, totUser, totElapsed :: !Double,
    generation :: !Int
  }
  deriving (Show)

instance Read Frame where
  readPrec = do
    spaces
    allocated <- readPrec @Int <* spaces
    copied <- readPrec @Int <* spaces
    live <- readPrec @Int <* spaces
    user <- readPrec @Double <* spaces
    elapsed <- readPrec @Double <* spaces
    totUser <- readPrec @Double <* spaces
    totElapsed <- readPrec @Double <* spaces
    _ <- readPrec @Int <* spaces
    _ <- readPrec @Int <* spaces
    "(Gen:  " <- replicateM 7 get
    generation <- readPrec @Int
    ')' <- get
    return Frame {..}
    where
      spaces = readP_to_Prec $ const P.skipSpaces

data TraceMetric = Allocated | Copied | Live | User | Elapsed
  deriving (Generic, Enum, Bounded, Read)

instance Show TraceMetric where
  show Allocated = "Allocated bytes"
  show Copied = "Copied bytes"
  show Live = "Live bytes"
  show User = "User time"
  show Elapsed = "Elapsed time"

frameMetric :: TraceMetric -> Frame -> Double
frameMetric Allocated = fromIntegral . allocated
frameMetric Copied = fromIntegral . copied
frameMetric Live = fromIntegral . live
frameMetric Elapsed = elapsed
frameMetric User = user

data Diagram = Diagram
  { traceMetric :: TraceMetric,
    runLogs :: [RunLog],
    title :: String
  }
  deriving (Generic)

-- | A file path containing the output of -S for a given run
data RunLog = RunLog
  { runVersion :: !String,
    _runExperiment :: !String,
    runFrames :: ![Frame]
  }

loadRunLog :: FilePath -> Escaped FilePath -> FilePath -> Action RunLog
loadRunLog buildF exp ver = do
  let fp = (buildF </> ver </> escaped exp <.> "benchmark-gcStats")
  need [fp]
  log <- liftIO $ lines <$> readFile fp
  let frames =
        [ f
          | l <- log,
            Just f <- [readMaybe l],
            -- filter out gen 0 events as there are too many
            generation f == 1
        ]
  return $ RunLog ver (dropExtension $ escaped exp) frames

plotDiagram :: Diagram -> FilePath -> Action ()
plotDiagram t@Diagram {traceMetric, runLogs} out = do
  let extract = frameMetric traceMetric
  liftIO $ E.toFile E.def out $ do
    E.layout_title .= title t
    forM_ runLogs $ \rl ->
      E.plot
        ( E.line
            (runVersion rl)
            [ [ (totElapsed f, extract f)
                | f <- runFrames rl
              ]
            ]
        )

s :: String -> String
s = id

(-/-) :: FilePattern -> FilePattern -> FilePattern
a -/- b = a <> "/" <> b

newtype Escaped a = Escaped {escaped :: a}

newtype Unescaped a = Unescaped {unescaped :: a}
  deriving newtype (Show, FromJSON, ToJSON, Eq, NFData, Binary, Hashable)

escapeExperiment :: Unescaped String -> Escaped String
escapeExperiment = Escaped . map f . unescaped
  where
    f ' ' = '_'
    f other = other

unescapeExperiment :: Escaped String -> Unescaped String
unescapeExperiment = Unescaped . map f . escaped
  where
    f '_' = ' '
    f other = other
