{-# LANGUAGE ExistentialQuantification #-}

{- An automated benchmark built around the simple experiment described in:

  > https://neilmitchell.blogspot.com/2020/05/fixing-space-leaks-in-ghcide.html

  As an example project, it unpacks Cabal-3.2.0.0 in the local filesystem and
  loads the module 'Distribution.Simple'. The rationale for this choice is:

    - It's convenient to download with `cabal unpack Cabal-3.2.0.0`
    - It has very few dependencies, and all are already needed to build ghcide
    - Distribution.Simple has 235 transitive module dependencies, so non trivial

  The experiments are sequences of lsp commands scripted using lsp-test.
  A more refined approach would be to record and replay real IDE interactions,
  once the replay functionality is available in lsp-test.
  A more declarative approach would be to reuse ide-debug-driver:

  > https://github.com/digital-asset/daml/blob/master/compiler/damlc/ide-debug-driver/README.md

  The result of an experiment is a total duration in seconds after a preset
  number of iterations. There is ample room for improvement:
     - Statistical analysis to detect outliers and auto infer the number of iterations needed
     - GC stats analysis (currently -S is printed as part of the experiment)
     - Analyisis of performance over the commit history of the project


 -}

import Control.Applicative.Combinators
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.Version
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities
import Numeric.Natural
import System.Directory
import System.FilePath ((</>))
import System.Process
import System.Time.Extra

examplePackageName :: String
examplePackageName = "Cabal"

examplePackageVersion :: Version
examplePackageVersion = makeVersion [3, 2, 0, 0]

examplePackage :: String
examplePackage = examplePackageName <> "-" <> showVersion examplePackageVersion

exampleModulePath :: FilePath
exampleModulePath = "Distribution" </> "Simple.hs"

-- For some reason, the Shake profile files are truncated and won't load
shakeProfiling :: Bool
shakeProfiling = False

main :: IO ()
main = do
  putStrLn "starting test"

  setup

  runBenchmarks
    [ bench "hover" 10 $ \doc ->
        isJust <$> getHover doc (Position 853 12),
      bench "getDefinition" 10 $ \doc ->
        not . null <$> getDefinitions doc (Position 853 12),
      bench "documentSymbols" 100 $
        fmap (either (not . null) (not . null)) . getDocumentSymbols,
      bench "documentSymbols after edit" 100 $ \doc -> do
        let change =
              TextDocumentContentChangeEvent
                { _range = Just (Range (Position 854 23) (Position 854 23)),
                  _rangeLength = Nothing,
                  _text = " "
                }
        changeDoc doc [change]
        either (not . null) (not . null) <$> getDocumentSymbols doc,
      bench "completions after edit" 10 $ \doc -> do
        let change =
              TextDocumentContentChangeEvent
                { _range = Just (Range (Position 854 23) (Position 854 23)),
                  _rangeLength = Nothing,
                  _text = " "
                }
        changeDoc doc [change]
        not . null <$> getCompletions doc (Position 853 12),
      benchWithSetup
        "code actions"
        10
        ( \doc -> do
            let p = Position 853 24
            let change =
                  TextDocumentContentChangeEvent
                    { _range = Just (Range p p),
                      _rangeLength = Nothing,
                      _text = "a"
                    }
            changeDoc doc [change]
            return p
        )
        ( \p doc -> do
            void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)
            not . null <$> getCodeActions doc (Range p p)
        ),
      bench "code actions after edit" 10 $ \doc -> do
        let p = Position 853 24
        let change =
              TextDocumentContentChangeEvent
                { _range = Just (Range p p),
                  _rangeLength = Nothing,
                  _text = "a"
                }
        changeDoc doc [change]
        void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)
        not . null <$> getCodeActions doc (Range p p)
    ]

type Experiment = TextDocumentIdentifier -> Session Bool

data Bench = forall setup.
  Bench
  { name :: !String,
    samples :: !Natural,
    benchSetup :: TextDocumentIdentifier -> Session setup,
    experiment :: setup -> Experiment
  }

bench :: String -> Natural -> Experiment -> Bench
bench name samples userExperiment = Bench {..}
  where
    experiment () = userExperiment
    benchSetup _ = return ()

benchWithSetup ::
  String ->
  Natural ->
  (TextDocumentIdentifier -> Session p) ->
  (p -> Experiment) ->
  Bench
benchWithSetup = Bench

runBenchmarks :: [Bench] -> IO ()
runBenchmarks benchmarks = do
  results <- forM benchmarks $ \b -> (b,) <$> runBench b

  forM_ results $ \(Bench {name, samples}, duration) ->
    putStrLn $
      "TOTAL "
        <> name
        <> " = "
        <> showDuration duration
        <> " ("
        <> show samples
        <> " repetitions)"

runBench :: Bench -> IO Seconds
runBench Bench {..} =
  runSessionWithConfig conf cmd lspTestCaps dir $ do
    doc <- openDoc exampleModulePath "haskell"
    void (skipManyTill anyMessage message :: Session WorkDoneProgressEndNotification)

    liftIO $ putStrLn $ "Running " <> name <> " benchmark"
    userState <- benchSetup doc
    (t, _) <- duration2 $ replicateM_ (fromIntegral samples) $ do
      (t, res) <- duration2 $ experiment userState doc
      unless res $ fail "DIDN'T WORK"
      liftIO $ putStrLn $ showDuration t

    exitServer
    -- sleeep to give ghcide a chance to print the RTS stats
    liftIO $ threadDelay 50000

    return t
  where
    cmd =
      unwords $
        [ "ghcide",
          "--lsp",
          "--test",
          "--cwd",
          dir,
          "+RTS",
          "-N4",
          "-S",
          "-RTS"
        ]
          ++ concat
            [ ["--shake-profiling", "shake-profiling"]
              | shakeProfiling
            ]
    dir = "bench/example/" <> examplePackage
    lspTestCaps =
      fullCaps {_window = Just $ WindowClientCapabilities $ Just True}
    conf =
      defaultConfig
        { logStdErr = True,
          logMessages = False,
          logColor = False
        }

setup :: IO ()
setup = do
  alreadyExists <- doesDirectoryExist "bench/example"
  when alreadyExists $ removeDirectoryRecursive "bench/example"
  callCommand $ "cabal unpack " <> examplePackage <> " -d bench/example"
  writeFile
    ("bench/example/" <> examplePackage <> "/hie.yaml")
    ("cradle: {cabal: {component: " <> show examplePackageName <> "}}")

  when shakeProfiling
    $ createDirectoryIfMissing True
    $ "bench/example" </> examplePackage </> "shake-profiling"

  -- print the path to ghcide (TODO platform independent)
  callCommand "which ghcide"

duration2 :: MonadIO m => m a -> m (Seconds, a)
duration2 x = do
  start <- liftIO offsetTime
  res <- x
  end <- liftIO start
  return (end, res)

-- | Asks the server to shutdown and exit politely
exitServer :: Session ()
exitServer = request_ Shutdown (Nothing :: Maybe Value) >> sendNotification Exit ExitParams
