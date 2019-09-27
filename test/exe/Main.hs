-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Development.IDE.Test
import Development.IDE.Test.Runfiles
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities
import System.Environment.Blank (setEnv)
import System.IO.Extra
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure


main :: IO ()
main = defaultMain $ testGroup "HIE"
  [ testSession "open close" $ do
      doc <- openDoc' "Testing.hs" "haskell" ""
      void (message :: Session ProgressStartNotification)
      closeDoc doc
      void (message :: Session ProgressDoneNotification)
  , diagnosticTests
  , codeActionTests
  , findDefinitionTests
  ]

diagnosticTests :: TestTree
diagnosticTests = testGroup "diagnostics"
  [ testSession "fix syntax error" $ do
      let content = T.unlines [ "module Testing wher" ]
      doc <- openDoc' "Testing.hs" "haskell" content
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "parse error")])]
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 19))
            , _rangeLength = Nothing
            , _text = "where"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [])]
  , testSession "introduce syntax error" $ do
      let content = T.unlines [ "module Testing where" ]
      doc <- openDoc' "Testing.hs" "haskell" content
      void (message :: Session ProgressStartNotification)
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 18))
            , _rangeLength = Nothing
            , _text = "wher"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "parse error")])]
  , testSession "variable not in scope" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int -> Int"
            , "foo a b = a + ab"
            , "bar :: Int -> Int -> Int"
            , "bar a b = cd + b"
            ]
      _ <- openDoc' "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [ (DsError, (2, 14), "Variable not in scope: ab")
            , (DsError, (4, 10), "Variable not in scope: cd")
            ]
          )
        ]
  , testSession "type error" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> String -> Int"
            , "foo a b = a + b"
            ]
      _ <- openDoc' "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [(DsError, (2, 14), "Couldn't match type '[Char]' with 'Int'")]
          )
        ]
  , testSession "typed hole" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> String"
            , "foo a = _ a"
            ]
      _ <- openDoc' "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [(DsError, (2, 8), "Found hole: _ :: Int -> String")]
          )
        ]

  , testGroup "deferral" $
    let sourceA a = T.unlines
          [ "module A where"
          , "a :: Int"
          , "a = " <> a]
        sourceB = T.unlines
          [ "module B where"
          , "import A"
          , "b :: Float"
          , "b = True"]
        bMessage = "Couldn't match expected type 'Float' with actual type 'Bool'"
        expectedDs aMessage =
          [ ("A.hs", [(DsError, (2,4), aMessage)])
          , ("B.hs", [(DsError, (3,4), bMessage)])]
        deferralTest title binding msg = testSession title $ do
          _ <- openDoc' "A.hs" "haskell" $ sourceA binding
          _ <- openDoc' "B.hs" "haskell"   sourceB
          expectDiagnostics $ expectedDs msg
    in
    [ deferralTest "type error"          "True"    "Couldn't match expected type"
    , deferralTest "typed hole"          "_"       "Found hole"
    , deferralTest "out of scope var"    "unbound" "Variable not in scope"
    , deferralTest "message shows error" "True"    "A.hs:3:5: error:"
    ]

  , testSession "remove required module" $ do
      let contentA = T.unlines [ "module ModuleA where" ]
      docA <- openDoc' "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 0) (Position 0 20))
            , _rangeLength = Nothing
            , _text = ""
            }
      changeDoc docA [change]
      expectDiagnostics [("ModuleB.hs", [(DsError, (1, 0), "Could not find module")])]
  , testSession "add missing module" $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
      expectDiagnostics [("ModuleB.hs", [(DsError, (1, 7), "Could not find module")])]
      let contentA = T.unlines [ "module ModuleA where" ]
      _ <- openDoc' "ModuleA.hs" "haskell" contentA
      expectDiagnostics [("ModuleB.hs", [])]
  , testSession "cyclic module dependency" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import ModuleB"
            ]
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- openDoc' "ModuleA.hs" "haskell" contentA
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
      expectDiagnostics
        [ ( "ModuleA.hs"
          , [(DsError, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")]
          )
        , ( "ModuleB.hs"
          , [(DsError, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")]
          )
        ]
  , testSession "cyclic module dependency with hs-boot" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import {-# SOURCE #-} ModuleB"
            ]
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      let contentBboot = T.unlines
            [ "module ModuleB where"
            ]
      _ <- openDoc' "ModuleA.hs" "haskell" contentA
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
      _ <- openDoc' "ModuleB.hs-boot" "haskell" contentBboot
      expectDiagnostics []
  , testSession "correct reference used with hs-boot" $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import {-# SOURCE #-} ModuleA"
            ]
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import ModuleB"
            , "x = 5"
            ]
      let contentAboot = T.unlines
            [ "module ModuleA where"
            ]
      let contentC = T.unlines
            [ "module ModuleC where"
            , "import ModuleA"
            -- this reference will fail if it gets incorrectly
            -- resolved to the hs-boot file
            , "y = x"
            ]
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
      _ <- openDoc' "ModuleA.hs" "haskell" contentA
      _ <- openDoc' "ModuleA.hs-boot" "haskell" contentAboot
      _ <- openDoc' "ModuleC.hs" "haskell" contentC
      expectDiagnostics []
  , testSession "redundant import" $ do
      let contentA = T.unlines ["module ModuleA where"]
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- openDoc' "ModuleA.hs" "haskell" contentA
      _ <- openDoc' "ModuleB.hs" "haskell" contentB
      expectDiagnostics
        [ ( "ModuleB.hs"
          , [(DsWarning, (2, 0), "The import of 'ModuleA' is redundant")]
          )
        ]
  , testSession "package imports" $ do
      let thisDataListContent = T.unlines
            [ "module Data.List where"
            , "x = 123"
            ]
      let mainContent = T.unlines
            [ "{-# LANGUAGE PackageImports #-}"
            , "module Main where"
            , "import qualified \"this\" Data.List as ThisList"
            , "import qualified \"base\" Data.List as BaseList"
            , "useThis = ThisList.x"
            , "useBase = BaseList.map"
            , "wrong1 = ThisList.map"
            , "wrong2 = BaseList.x"
            ]
      _ <- openDoc' "Data/List.hs" "haskell" thisDataListContent
      _ <- openDoc' "Main.hs" "haskell" mainContent
      expectDiagnostics
        [ ( "Main.hs"
          , [(DsError, (6, 9), "Not in scope: \8216ThisList.map\8217")
            ,(DsError, (7, 9), "Not in scope: \8216BaseList.x\8217")
            ]
          )
        ]
  , testSession "unqualified warnings" $ do
      let fooContent = T.unlines
            [ "{-# OPTIONS_GHC -Wredundant-constraints #-}"
            , "module Foo where"
            , "foo :: Ord a => a -> Int"
            , "foo a = 1"
            ]
      _ <- openDoc' "Foo.hs" "haskell" fooContent
      expectDiagnostics
        [ ( "Foo.hs"
      -- The test is to make sure that warnings contain unqualified names
      -- where appropriate. The warning should use an unqualified name 'Ord', not
      -- sometihng like 'GHC.Classes.Ord'. The choice of redundant-constraints to
      -- test this is fairly arbitrary.
          , [(DsWarning, (2, 0), "Redundant constraint: Ord a")
            ]
          )
        ]
  ]

codeActionTests :: TestTree
codeActionTests = testGroup "code actions"
  [ renameActionTests
  , typeWildCardActionTests
  , removeImportTests
  , importRenameActionTests
  , fillTypedHoleTests
  , addSigActionTests
  ]

renameActionTests :: TestTree
renameActionTests = testGroup "rename actions"
  [ testSession "change to local variable name" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int"
            , "foo argName = argNme"
            ]
      doc <- openDoc' "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }]
          <- getCodeActions doc (Range (Position 2 14) (Position 2 20))
      liftIO $ "Replace with ‘argName’" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int"
            , "foo argName = argName"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "change to name of imported function" $ do
      let content = T.unlines
            [ "module Testing where"
            , "import Data.Maybe (maybeToList)"
            , "foo :: Maybe a -> [a]"
            , "foo = maybToList"
            ]
      doc <- openDoc' "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }]
          <- getCodeActions doc (Range (Position 3 6) (Position 3 16))
      liftIO $ "Replace with ‘maybeToList’" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "import Data.Maybe (maybeToList)"
            , "foo :: Maybe a -> [a]"
            , "foo = maybeToList"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "suggest multiple local variable names" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Char -> Char -> Char -> Char"
            , "foo argument1 argument2 argument3 = argumentX"
            ]
      doc <- openDoc' "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 2 36) (Position 2 45))
      let actionTitles = [ actionTitle | CACodeAction CodeAction{ _title = actionTitle } <- actionsOrCommands ]
          expectedActionTitles = ["Replace with ‘argument1’", "Replace with ‘argument2’", "Replace with ‘argument3’"]
      liftIO $ expectedActionTitles @=? actionTitles
  , testSession "change infix function" $ do
      let content = T.unlines
            [ "module Testing where"
            , "monus :: Int -> Int"
            , "monus x y = max 0 (x - y)"
            , "foo x y = x `monnus` y"
            ]
      doc <- openDoc' "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 3 12) (Position 3 20))
      [fixTypo] <- pure [action | CACodeAction action@CodeAction{ _title = actionTitle } <- actionsOrCommands, "monus" `T.isInfixOf` actionTitle ]
      executeCodeAction fixTypo
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "monus :: Int -> Int"
            , "monus x y = max 0 (x - y)"
            , "foo x y = x `monus` y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

typeWildCardActionTests :: TestTree
typeWildCardActionTests = testGroup "type wildcard actions"
  [ testSession "global signature" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: _"
            , "func x = x"
            ]
      doc <- openDoc' "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 2 1) (Position 2 10))
      let [addSignature] = [action | CACodeAction action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                   , "Use type signature" `T.isInfixOf` actionTitle
                           ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: (p -> p)"
            , "func x = x"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "multi-line message" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: _"
            , "func x y = x + y"
            ]
      doc <- openDoc' "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 2 1) (Position 2 10))
      let [addSignature] = [action | CACodeAction action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                    , "Use type signature" `T.isInfixOf` actionTitle
                              ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: (Integer -> Integer -> Integer)"
            , "func x y = x + y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "local signature" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: Int -> Int"
            , "func x ="
            , "  let y :: _"
            , "      y = x * 2"
            , "  in y"
            ]
      doc <- openDoc' "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 4 1) (Position 4 10))
      let [addSignature] = [action | CACodeAction action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                    , "Use type signature" `T.isInfixOf` actionTitle
                              ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: Int -> Int"
            , "func x ="
            , "  let y :: (Int)"
            , "      y = x * 2"
            , "  in y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

removeImportTests :: TestTree
removeImportTests = testGroup "remove import actions"
  [ testSession "redundant" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            ]
      _docA <- openDoc' "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA"
            , "stuffB = 123"
            ]
      docB <- openDoc' "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "qualified redundant" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            ]
      _docA <- openDoc' "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA"
            , "stuffB = 123"
            ]
      docB <- openDoc' "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [CACodeAction action@CodeAction { _title = actionTitle }]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

importRenameActionTests :: TestTree
importRenameActionTests = testGroup "import rename actions"
  [ testSession "Data.Mape -> Data.Map"   $ check "Map"
  , testSession "Data.Mape -> Data.Maybe" $ check "Maybe" ] where
  check modname = do
      let content = T.unlines
            [ "module Testing where"
            , "import Data.Mape"
            ]
      doc <- openDoc' "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 2 8) (Position 2 16))
      let [changeToMap] = [action | CACodeAction action@CodeAction{ _title = actionTitle } <- actionsOrCommands, ("Data." <> modname) `T.isInfixOf` actionTitle ]
      executeCodeAction changeToMap
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "import Data." <> modname
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction

fillTypedHoleTests :: TestTree
fillTypedHoleTests = let

  sourceCode :: T.Text -> T.Text -> T.Text -> T.Text
  sourceCode a b c = T.unlines
    [ "module Testing where"
      , ""
      , "globalConvert :: Int -> String"
      , "globalConvert = undefined"
      , ""
      , "globalInt :: Int"
      , "globalInt = 3"
      , ""
      , "bar :: Int -> Int -> String"
      , "bar n parameterInt = " <> a <> " (n + " <> b <> " + " <> c <> ")  where"
      , "  localConvert = (flip replicate) 'x'"

    ]

  check :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle
        oldA oldB oldC
        newA newB newC = testSession (T.unpack actionTitle) $ do
    let originalCode = sourceCode oldA oldB oldC
    let expectedCode = sourceCode newA newB newC
    doc <- openDoc' "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 9 0) (Position 9 maxBound))
    let chosenAction = pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "fill typed holes"
  [ check "replace hole `_` with show"
          "_"    "n" "n"
          "show" "n" "n"

  , check "replace hole `_` with globalConvert"
          "_"             "n" "n"
          "globalConvert" "n" "n"

#if MIN_GHC_API_VERSION(8,6,0)
  , check "replace hole `_convertme` with localConvert"
          "_convertme"   "n" "n"
          "localConvert" "n" "n"
#endif

  , check "replace hole `_b` with globalInt"
          "_a" "_b"        "_c"
          "_a" "globalInt" "_c"

  , check "replace hole `_c` with globalInt"
          "_a" "_b"        "_c"
          "_a" "_b" "globalInt"

#if MIN_GHC_API_VERSION(8,6,0)
  , check "replace hole `_c` with parameterInt"
          "_a" "_b" "_c"
          "_a" "_b"  "parameterInt"
#endif
  ]

addSigActionTests :: TestTree
addSigActionTests = let
  header = T.unlines [ "{-# OPTIONS_GHC -Wmissing-signatures #-}"
                     , "module Sigs where"]
  before  def     = T.unlines [header,      def]
  after'  def sig = T.unlines [header, sig, def]

  def >:: sig = testSession (T.unpack def) $ do
    let originalCode = before def
    let expectedCode = after'  def sig
    doc <- openDoc' "Sigs.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 3 1) (Position 3 maxBound))
    let chosenAction = pickActionWithTitle ("add signature: " <> sig) actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "add signature"
  [ "abc = True"             >:: "abc :: Bool"
  , "foo a b = a + b"        >:: "foo :: Num a => a -> a -> a"
  , "bar a b = show $ a + b" >:: "bar :: (Show a, Num a) => a -> a -> String"
  , "(!!!) a b = a > b"      >:: "(!!!) :: Ord a => a -> a -> Bool"
  , "a >>>> b = a + b"       >:: "(>>>>) :: Num a => a -> a -> a"
  , "a `haha` b = a b"       >:: "haha :: (t1 -> t2) -> t1 -> t2"
  ]

findDefinitionTests :: TestTree
findDefinitionTests = let

  tst (get, check) pos targetRange title = testSession title $ do
    doc <- openDoc' "Testing.hs" "haskell" source
    found <- get doc pos
    check found targetRange

  checkDefs defs expected = do

    let ndef = length defs
    if ndef /= 1
      then let dfound n = "definitions found: " <> show n in
           liftIO $ dfound (1 :: Int) @=? dfound (length defs)
      else do
           let [Location{_range = foundRange}] = defs
           liftIO $ expected @=? foundRange

  checkHover hover expected = do
    case hover of
      Nothing -> liftIO $ "hover found" @=? ("no hover found" :: T.Text)
      Just Hover{_contents = (HoverContents MarkupContent{_value = v})} ->
        liftIO $ adjust expected @=? Position l c where
          found = T.splitOn ":" $ head $ T.splitOn "**" $ last $ T.splitOn "Testing.hs:" v
          [l,c] =  map (read . T.unpack) found
          -- looks like hovers use 1-based numbering while definitions use 0-based
          adjust Range{_start = Position{_line = l, _character = c}} =
            Position{_line = l + 1, _character = c + 1}

  source = T.unlines
    -- 0123456789 123456789 123456789 123456789
    [ "{-# OPTIONS_GHC -Wmissing-signatures #-}" --  0
    , "module Testing where"                     --  1
    , "data TypeConstructor = DataConstructor"   --  2
    , "  { fff :: String"                        --  3
    , "  , ggg :: Int }"                         --  4
    , "aaa :: TypeConstructor"                   --  5
    , "aaa = DataConstructor"                    --  6
    , "  { fff = \"\""                           --  7
    , "  , ggg = 0"                              --  8
    , "  }"                                      --  9
    -- 0123456789 123456789 123456789 123456789
    , "bbb :: TypeConstructor"                   -- 10
    , "bbb = DataConstructor \"\" 0"             -- 11
    , "ccc :: (String, Int)"                     -- 12
    , "ccc = (fff bbb, ggg aaa)"                 -- 13
    , "ddd :: Num a => a -> a -> a"              -- 14
    , "ddd vv ww = vv +! ww"                     -- 15
    , "a +! b = a - b"                           -- 16
    , "hhh (Just a) (><) = a >< a"               -- 17
    , "iii a b = a `b` a"                        -- 18
    -- 0123456789 123456789 123456789 123456789
    ]

  -- definition locations
  tcData = mkRange   2  0    4 16
  tcDC   = mkRange   2 23    4 16
  fff    = mkRange   3  4    3  7
  aaa    = mkRange   6  0    6  3
  vv     = mkRange  15  4   15  6
  op     = mkRange  16  2   16  4
  opp    = mkRange  17 13   17 15
  apmp   = mkRange  17 10   17 11
  bp     = mkRange  18  6   18  7
  -- search locations
  fffL3  = _start fff
  fffL7  = Position  7  4
  fffL13 = Position 13  7
  aaaL13 = Position 13 20
  dcL6   = Position  6 11
  dcL11  = Position 11 11
  tcL5   = Position  5 11
  vvL15  = Position 15 12
  opL15  = Position 15 15
  opL17  = Position 17 21
  aL17   = Position 17 20
  b'L18  = Position 18 13

  --t = (getTypeDefinitions, checkTDefs) -- getTypeDefinitions always times out
  d = (getDefinitions, checkDefs)
  h = (getHover, checkHover)
  in
  testGroup "get"
  [ testGroup "definition"
    [ tst d fffL3  fff    "field in record definition"
    , tst d fffL7  fff    "field in record construction" `xfail` "known broken"
    , tst d fffL13 fff    "field name used as accessor"   -- 120 in Calculate.hs
    , tst d aaaL13 aaa    "top-level name"                -- 120
    , tst d dcL6   tcDC   "record data constructor"      `xfail` "known broken"
    , tst d dcL11  tcDC   "plain  data constructor"       -- 121
    , tst d tcL5   tcData "type constructor"              -- 147
    , tst d vvL15  vv     "plain parameter"
    , tst d aL17   apmp   "pattern match name"
    , tst d opL15  op     "top-level operator"
    , tst d opL17  opp    "parameter operator"           `xfail` "known broken"
    , tst d b'L18  bp     "name in backticks"
    ]
  , testGroup "hover"
    [ tst h fffL3  fff    "field in record definition"
    , tst h fffL7  fff    "field in record construction" `xfail` "known broken"
    , tst h fffL13 fff    "field name used as accessor"   -- 120
    , tst h aaaL13 aaa    "top-level name"                -- 120
    , tst h dcL6   tcDC   "record data constructor"      `xfail` "known broken"
    , tst h dcL11  tcDC   "plain  data constructor"       -- 121
    , tst h tcL5   tcData "type constructor"             `xfail` "known broken"
    , tst h vvL15  vv     "plain parameter"
    , tst h aL17   apmp   "pattern match name"
    , tst h opL15  op     "top-level operator"
    , tst d opL17  opp    "parameter operator"           `xfail` "known broken"
    , tst h b'L18  bp     "name in backticks"
    ]
  ]

xfail :: TestTree -> String -> TestTree
xfail = flip expectFailBecause

----------------------------------------------------------------------
-- Utils


testSession :: String -> Session () -> TestTree
testSession name =
  testCase name . run .
      -- Check that any diagnostics produced were already consumed by the test case.
      --
      -- If in future we add test cases where we don't care about checking the diagnostics,
      -- this could move elsewhere.
      --
      -- Experimentally, 0.5s seems to be long enough to wait for any final diagnostics to appear.
      ( >> expectNoMoreDiagnostics 0.5)

pickActionWithTitle :: T.Text -> [CAResult] -> CodeAction
pickActionWithTitle title actions = head
  [ action
  | CACodeAction action@CodeAction{ _title = actionTitle } <- actions
  , title == actionTitle ]

mkRange :: Int -> Int -> Int -> Int -> Range
mkRange a b c d = Range (Position a b) (Position c d)

run :: Session a -> IO a
run s = withTempDir $ \dir -> do
  ghcideExe <- locateGhcideExecutable
  let cmd = unwords [ghcideExe, "--lsp", "--cwd", dir]
  -- HIE calls getXgdDirectory which assumes that HOME is set.
  -- Only sets HOME if it wasn't already set.
  setEnv "HOME" "/homeless-shelter" False
  runSessionWithConfig conf cmd fullCaps { _window = Just $ WindowClientCapabilities $ Just True } dir s
  where
    conf = defaultConfig
      -- If you uncomment this you can see all messages
      -- which can be quite useful for debugging.
      -- { logMessages = True, logColor = False, logStdErr = True }
