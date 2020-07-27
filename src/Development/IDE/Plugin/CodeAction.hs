-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

-- | Go to the definition of a variable.
module Development.IDE.Plugin.CodeAction
    (
      plugin

    -- * For haskell-language-server
    , codeAction
    , codeLens
    , rulePackageExports
    , executeAddSignatureCommand
    ) where

import Control.Monad (join)
import Development.IDE.Plugin
import Development.IDE.GHC.Compat
import Development.IDE.Core.Rules
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.IDE.GHC.Error
import Development.IDE.GHC.Util
import Development.IDE.LSP.Server
import Development.IDE.Plugin.CodeAction.PositionIndexed
import Development.IDE.Plugin.CodeAction.RuleTypes
import Development.IDE.Plugin.CodeAction.Rules
import Development.IDE.Types.Location
import Development.IDE.Types.Options
import Development.Shake (Rules)
import qualified Data.HashMap.Strict as Map
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import qualified Data.Rope.UTF16 as Rope
import Data.Aeson.Types (toJSON, fromJSON, Value(..), Result(..))
import Data.Char
import Data.Maybe
import Data.List.Extra
import qualified Data.Text as T
import Data.Tuple.Extra ((&&&))
import HscTypes
import SrcLoc (sortLocated)
import Parser
import Text.Regex.TDFA ((=~), (=~~))
import Text.Regex.TDFA.Text()
import Outputable (ppr, showSDocUnsafe)
import DynFlags (xFlags, FlagSpec(..))
import GHC.LanguageExtensions.Type (Extension)
import Data.Function
import Control.Arrow ((>>>))
import Data.Functor
import Control.Applicative ((<|>))

plugin :: Plugin c
plugin = codeActionPluginWithRules rules codeAction <> Plugin mempty setHandlersCodeLens

rules :: Rules ()
rules = rulePackageExports

-- | Generate code actions.
codeAction
    :: LSP.LspFuncs c
    -> IdeState
    -> TextDocumentIdentifier
    -> Range
    -> CodeActionContext
    -> IO (Either ResponseError [CAResult])
codeAction lsp state (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics=List xs} = do
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    let text = Rope.toText . (_text :: VirtualFile -> Rope.Rope) <$> contents
        mbFile = toNormalizedFilePath' <$> uriToFilePath uri
    (ideOptions, parsedModule, join -> env) <- runAction "CodeAction" state $
      (,,) <$> getIdeOptions
            <*> getParsedModule `traverse` mbFile
            <*> use GhcSession `traverse` mbFile
    -- This is quite expensive 0.6-0.7s on GHC
    pkgExports <- runAction "CodeAction:PackageExports" state $ (useNoFile_ . PackageExports) `traverse` env
    let dflags = hsc_dflags . hscEnv <$> env
    pure $ Right
        [ CACodeAction $ CodeAction title (Just CodeActionQuickFix) (Just $ List [x]) (Just edit) Nothing
        | x <- xs, (title, tedit) <- suggestAction dflags (fromMaybe mempty pkgExports) ideOptions ( join parsedModule ) text x
        , let edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
        ]

-- | Generate code lenses.
codeLens
    :: LSP.LspFuncs c
    -> IdeState
    -> CodeLensParams
    -> IO (Either ResponseError (List CodeLens))
codeLens _lsp ideState CodeLensParams{_textDocument=TextDocumentIdentifier uri} = do
    commandId <- makeLspCommandId "typesignature.add"
    fmap (Right . List) $ case uriToFilePath' uri of
      Just (toNormalizedFilePath' -> filePath) -> do
        _ <- runAction "codeLens" ideState (use TypeCheck filePath)
        diag <- getDiagnostics ideState
        hDiag <- getHiddenDiagnostics ideState
        pure
          [ CodeLens _range (Just (Command title commandId (Just $ List [toJSON edit]))) Nothing
          | (dFile, _, dDiag@Diagnostic{_range=_range}) <- diag ++ hDiag
          , dFile == filePath
          , (title, tedit) <- suggestSignature False dDiag
          , let edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
          ]
      Nothing -> pure []

-- | Execute the "typesignature.add" command.
executeAddSignatureCommand
    :: LSP.LspFuncs c
    -> IdeState
    -> ExecuteCommandParams
    -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
executeAddSignatureCommand _lsp _ideState ExecuteCommandParams{..}
    -- _command is prefixed with a process ID, because certain clients
    -- have a global command registry, and all commands must be
    -- unique. And there can be more than one ghcide instance running
    -- at a time against the same client.
    | T.isSuffixOf "typesignature.add" _command
    , Just (List [edit]) <- _arguments
    , Success wedit <- fromJSON edit
    = return (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams wedit))
    | otherwise
    = return (Right Null, Nothing)

suggestAction
  :: Maybe DynFlags
  -> PackageExportsMap
  -> IdeOptions
  -> Maybe ParsedModule
  -> Maybe T.Text
  -> Diagnostic
  -> [(T.Text, [TextEdit])]
suggestAction dflags packageExports ideOptions parsedModule text diag = concat
    [ suggestAddExtension diag
    , suggestExtendImport dflags text diag
    , suggestFillHole diag
    , suggestFillTypeWildcard diag
    , suggestFixConstructorImport text diag
    , suggestModuleTypo diag
    , suggestReplaceIdentifier text diag
    , suggestSignature True diag
    , suggestConstraint text diag
    , suggestAddTypeAnnotationToSatisfyContraints text diag
    ] ++ concat
    [  suggestNewDefinition ideOptions pm text diag
    ++ suggestRemoveRedundantImport pm text diag
    ++ suggestNewImport packageExports pm diag
    ++ suggestDeleteTopBinding pm diag
    ++ suggestExportUnusedTopBinding pm diag
    | Just pm <- [parsedModule]]


suggestRemoveRedundantImport :: ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestRemoveRedundantImport ParsedModule{pm_parsed_source = L _  HsModule{hsmodImports}} contents Diagnostic{_range=_range,..}
--     The qualified import of ‘many’ from module ‘Control.Applicative’ is redundant
    | Just [_, bindings] <- matchRegex _message "The( qualified)? import of ‘([^’]*)’ from module [^ ]* is redundant"
    , Just (L _ impDecl) <- find (\(L l _) -> srcSpanToRange l == _range ) hsmodImports
    , Just c <- contents
    , ranges <- map (rangesForBinding impDecl . T.unpack) (T.splitOn ", " bindings)
    , ranges' <- extendAllToIncludeCommaIfPossible (indexedByPosition $ T.unpack c) (concat ranges)
    , not (null ranges')
    = [( "Remove " <> bindings <> " from import" , [ TextEdit r "" | r <- ranges' ] )]

-- File.hs:16:1: warning:
--     The import of `Data.List' is redundant
--       except perhaps to import instances from `Data.List'
--     To import instances alone, use: import Data.List()
    | _message =~ ("The( qualified)? import of [^ ]* is redundant" :: String)
        = [("Remove import", [TextEdit (extendToWholeLineIfPossible contents _range) ""])]
    | otherwise = []

suggestDeleteTopBinding :: ParsedModule -> Diagnostic -> [(T.Text, [TextEdit])]
suggestDeleteTopBinding ParsedModule{pm_parsed_source = L _ HsModule{hsmodDecls}} Diagnostic{_range=_range,..}
-- Foo.hs:4:1: warning: [-Wunused-top-binds] Defined but not used: ‘f’
    | Just [name] <- matchRegex _message ".*Defined but not used: ‘([^ ]+)’"
    , let allTopLevel = filter (isTopLevel . fst)
                        . map (\(L l b) -> (srcSpanToRange l, b))
                        . sortLocated
                        $ hsmodDecls
          sameName = filter (matchesBindingName (T.unpack name) . snd) allTopLevel
    , not (null sameName)
            = [("Delete ‘" <> name <> "’", flip TextEdit "" . toNextBinding allTopLevel . fst <$> sameName )]
    | otherwise = []
    where
      isTopLevel l = (_character . _start) l == 0

      forwardLines lines r = r {_end = (_end r) {_line = (_line . _end $ r) + lines, _character = 0}}

      toNextBinding bindings r@Range { _end = Position {_line = l} }
        | Just (Range { _start = Position {_line = l'}}, _) <- find ((> l) . _line . _start . fst) bindings
        = forwardLines (l' - l) r
      toNextBinding _ r  = r

      matchesBindingName :: String -> HsDecl GhcPs -> Bool
      matchesBindingName b (ValD FunBind {fun_id=L _ x}) = showSDocUnsafe (ppr x) == b
      matchesBindingName b (SigD (TypeSig (L _ x:_) _)) = showSDocUnsafe (ppr x) == b
      matchesBindingName _ _ = False

data ExportsAs = ExportName | ExportPattern | ExportAll
  deriving (Eq)

suggestExportUnusedTopBinding :: ParsedModule -> Diagnostic -> [(T.Text, [TextEdit])]
suggestExportUnusedTopBinding ParsedModule{pm_parsed_source = L _ HsModule{..}} Diagnostic{..}
-- Foo.hs:4:1: warning: [-Wunused-top-binds] Defined but not used: ‘f’
-- Foo.hs:5:1: warning: [-Wunused-top-binds] Defined but not used: type constructor or class ‘F’
-- Foo.hs:6:1: warning: [-Wunused-top-binds] Defined but not used: data constructor ‘Bar’
  | Just [name] <- matchRegex _message ".*Defined but not used: ‘([^ ]+)’"
                   <|> matchRegex _message ".*Defined but not used: type constructor or class ‘([^ ]+)’"
                   <|> matchRegex _message ".*Defined but not used: data constructor ‘([^ ]+)’"
  , Just (exportType, _) <- listToMaybe
                            . filter (matchWithDiagnostic _range . snd)
                            . catMaybes
                            . map (\(L l b) -> if isTopLevel $ srcSpanToRange l
                                                  then exportsAs b else Nothing)
                            $ hsmodDecls
  , Just pos <- _start . getLocatedRange <$> hsmodExports
  , Just needComma <- not . null . unLoc <$> hsmodExports
  , let exportName = printExport exportType name <> (if needComma then "," else "")
        insertPos = pos {_character = succ $ _character pos}
  = [("Export ‘" <> name <> "’", [TextEdit (Range insertPos insertPos) exportName])]
  | otherwise = []
  where
    getLocatedRange :: Located a -> Range
    getLocatedRange = srcSpanToRange . getLoc

    matchWithDiagnostic :: Range -> Located (IdP GhcPs) -> Bool
    matchWithDiagnostic Range{_start=l,_end=r} x =
      let loc = _start . getLocatedRange $ x
       in loc >= l && loc <= r

    printExport :: ExportsAs -> T.Text -> T.Text
    printExport ExportName x = x
    printExport ExportPattern x = "pattern " <> x
    printExport ExportAll x = x <> "(..)"

    isTopLevel :: Range -> Bool
    isTopLevel l = (_character . _start) l == 0

    exportsAs :: HsDecl p -> Maybe (ExportsAs, Located (IdP p))
    exportsAs (ValD FunBind {fun_id})          = Just $ (ExportName, fun_id)
    exportsAs (ValD (PatSynBind PSB {psb_id})) = Just $ (ExportPattern, psb_id)
    exportsAs (TyClD (SynDecl{tcdLName}))      = Just $ (ExportName, tcdLName)
    exportsAs (TyClD (DataDecl{tcdLName}))     = Just $ (ExportAll, tcdLName)
    exportsAs (TyClD (ClassDecl{tcdLName}))    = Just $ (ExportAll, tcdLName)
    exportsAs (TyClD (FamDecl{tcdFam}))        = Just $ (ExportAll, fdLName tcdFam)
    exportsAs _                                = Nothing

suggestAddTypeAnnotationToSatisfyContraints :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestAddTypeAnnotationToSatisfyContraints sourceOpt Diagnostic{_range=_range,..}
-- File.hs:52:41: warning:
--     * Defaulting the following constraint to type ‘Integer’
--        Num p0 arising from the literal ‘1’
--     * In the expression: 1
--       In an equation for ‘f’: f = 1
-- File.hs:52:41: warning:
--     * Defaulting the following constraints to type ‘[Char]’
--        (Show a0)
--          arising from a use of ‘traceShow’
--          at A.hs:228:7-25
--        (IsString a0)
--          arising from the literal ‘"debug"’
--          at A.hs:228:17-23
--     * In the expression: traceShow "debug" a
--       In an equation for ‘f’: f a = traceShow "debug" a
-- File.hs:52:41: warning:
--     * Defaulting the following constraints to type ‘[Char]’
--         (Show a0)
--          arising from a use of ‘traceShow’
--          at A.hs:255:28-43
--        (IsString a0)
--          arising from the literal ‘"test"’
--          at /Users/serhiip/workspace/ghcide/src/Development/IDE/Plugin/CodeAction.hs:255:38-43
--     * In the fourth argument of ‘seq’, namely ‘(traceShow "test")’
--       In the expression: seq "test" seq "test" (traceShow "test")
--       In an equation for ‘f’:
--          f = seq "test" seq "test" (traceShow "test")
    | Just [ty, lit] <- matchRegex _message (pat False False True)
                        <|> matchRegex _message (pat False False False)
            = codeEdit ty lit (makeAnnotatedLit ty lit)
    | Just source <- sourceOpt
    , Just [ty, lit] <- matchRegex _message (pat True True False)
            = let lit' = makeAnnotatedLit ty lit;
                  tir = textInRange _range source
              in codeEdit ty lit (T.replace lit lit' tir)
    | otherwise = []
    where
      makeAnnotatedLit ty lit = "(" <> lit <> " :: " <> ty <> ")"
      pat multiple at inThe = T.concat [ ".*Defaulting the following constraint"
                                       , if multiple then "s" else ""
                                       , " to type ‘([^ ]+)’ "
                                       , ".*arising from the literal ‘(.+)’"
                                       , if inThe then ".+In the.+argument" else ""
                                       , if at then ".+at" else ""
                                       , ".+In the expression"
                                       ]
      codeEdit ty lit replacement =
        let title = "Add type annotation ‘" <> ty <> "’ to ‘" <> lit <> "’"
            edits = [TextEdit _range replacement]
        in  [( title, edits )]


suggestReplaceIdentifier :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestReplaceIdentifier contents Diagnostic{_range=_range,..}
-- File.hs:52:41: error:
--     * Variable not in scope:
--         suggestAcion :: Maybe T.Text -> Range -> Range
--     * Perhaps you meant ‘suggestAction’ (line 83)
-- File.hs:94:37: error:
--     Not in scope: ‘T.isPrfixOf’
--     Perhaps you meant one of these:
--       ‘T.isPrefixOf’ (imported from Data.Text),
--       ‘T.isInfixOf’ (imported from Data.Text),
--       ‘T.isSuffixOf’ (imported from Data.Text)
--     Module ‘Data.Text’ does not export ‘isPrfixOf’.
    | renameSuggestions@(_:_) <- extractRenamableTerms _message
        = [ ("Replace with ‘" <> name <> "’", [mkRenameEdit contents _range name]) | name <- renameSuggestions ]
    | otherwise = []

suggestNewDefinition :: IdeOptions -> ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestNewDefinition ideOptions parsedModule contents Diagnostic{_message, _range}
--     * Variable not in scope:
--         suggestAcion :: Maybe T.Text -> Range -> Range
    | Just [name, typ] <- matchRegex message "Variable not in scope: ([^ ]+) :: ([^*•]+)"
    = newDefinitionAction ideOptions parsedModule _range name typ
    | Just [name, typ] <- matchRegex message "Found hole: _([^ ]+) :: ([^*•]+) Or perhaps"
    , [(label, newDefinitionEdits)] <- newDefinitionAction ideOptions parsedModule _range name typ
    = [(label, mkRenameEdit contents _range name : newDefinitionEdits)]
    | otherwise = []
    where
      message = unifySpaces _message

newDefinitionAction :: IdeOptions -> ParsedModule -> Range -> T.Text -> T.Text -> [(T.Text, [TextEdit])]
newDefinitionAction IdeOptions{..} parsedModule Range{_start} name typ
    | Range _ lastLineP : _ <-
      [ srcSpanToRange l
      | (L l _) <- hsmodDecls
      , _start `isInsideSrcSpan` l]
    , nextLineP <- Position{ _line = _line lastLineP + 1, _character = 0}
    = [ ("Define " <> sig
        , [TextEdit (Range nextLineP nextLineP) (T.unlines ["", sig, name <> " = error \"not implemented\""])]
        )]
    | otherwise = []
  where
    colon = if optNewColonConvention then " : " else " :: "
    sig = name <> colon <> T.dropWhileEnd isSpace typ
    ParsedModule{pm_parsed_source = L _ HsModule{hsmodDecls}} = parsedModule


suggestFillTypeWildcard :: Diagnostic -> [(T.Text, [TextEdit])]
suggestFillTypeWildcard Diagnostic{_range=_range,..}
-- Foo.hs:3:8: error:
--     * Found type wildcard `_' standing for `p -> p1 -> p'

    | "Found type wildcard" `T.isInfixOf` _message
    , " standing for " `T.isInfixOf` _message
    , typeSignature <- extractWildCardTypeSignature _message
        =  [("Use type signature: ‘" <> typeSignature <> "’", [TextEdit _range typeSignature])]
    | otherwise = []

suggestAddExtension :: Diagnostic -> [(T.Text, [TextEdit])]
suggestAddExtension Diagnostic{_range=_range,..}
-- File.hs:22:8: error:
--     Illegal lambda-case (use -XLambdaCase)
-- File.hs:22:6: error:
--     Illegal view pattern:  x -> foo
--     Use ViewPatterns to enable view patterns
-- File.hs:26:8: error:
--     Illegal `..' in record pattern
--     Use RecordWildCards to permit this
-- File.hs:53:28: error:
--     Illegal tuple section: use TupleSections
-- File.hs:238:29: error:
--     * Can't make a derived instance of `Data FSATrace':
--         You need DeriveDataTypeable to derive an instance for this class
--     * In the data declaration for `FSATrace'
-- C:\Neil\shake\src\Development\Shake\Command.hs:515:31: error:
--     * Illegal equational constraint a ~ ()
--       (Use GADTs or TypeFamilies to permit this)
--     * In the context: a ~ ()
--       While checking an instance declaration
--       In the instance declaration for `Unit (m a)'
    | exts@(_:_) <- filter (`Map.member` ghcExtensions) $ T.split (not . isAlpha) $ T.replace "-X" "" _message
        = [("Add " <> x <> " extension", [TextEdit (Range (Position 0 0) (Position 0 0)) $ "{-# LANGUAGE " <> x <> " #-}\n"]) | x <- exts]
    | otherwise = []

-- | All the GHC extensions
ghcExtensions :: Map.HashMap T.Text Extension
ghcExtensions = Map.fromList . filter notStrictFlag . map ( ( T.pack . flagSpecName ) &&& flagSpecFlag ) $ xFlags
  where
    -- Strict often causes false positives, as in Data.Map.Strict imports.
    -- See discussion at https://github.com/digital-asset/ghcide/pull/638
    notStrictFlag (name, _) = name /= "Strict"

suggestModuleTypo :: Diagnostic -> [(T.Text, [TextEdit])]
suggestModuleTypo Diagnostic{_range=_range,..}
-- src/Development/IDE/Core/Compile.hs:58:1: error:
--     Could not find module ‘Data.Cha’
--     Perhaps you meant Data.Char (from base-4.12.0.0)
    | "Could not find module" `T.isInfixOf` _message
    , "Perhaps you meant"     `T.isInfixOf` _message = let
      findSuggestedModules = map (head . T.words) . drop 2 . T.lines
      proposeModule mod = ("replace with " <> mod, [TextEdit _range mod])
      in map proposeModule $ nubOrd $ findSuggestedModules _message
    | otherwise = []

suggestFillHole :: Diagnostic -> [(T.Text, [TextEdit])]
suggestFillHole Diagnostic{_range=_range,..}
--  ...Development/IDE/LSP/CodeAction.hs:103:9: warning:
--   * Found hole: _ :: Int -> String
--   * In the expression: _
--     In the expression: _ a
--     In an equation for ‘foo’: foo a = _ a
--   * Relevant bindings include
--       a :: Int
--         (bound at ...Development/IDE/LSP/CodeAction.hs:103:5)
--       foo :: Int -> String
--         (bound at ...Development/IDE/LSP/CodeAction.hs:103:1)
--     Valid hole fits include
--       foo :: Int -> String
--         (bound at ...Development/IDE/LSP/CodeAction.hs:103:1)
--       show :: forall a. Show a => a -> String
--         with show @Int
--         (imported from ‘Prelude’ at ...Development/IDE/LSP/CodeAction.hs:7:8-37
--          (and originally defined in ‘GHC.Show’))
--       mempty :: forall a. Monoid a => a
--         with mempty @(Int -> String)
--         (imported from ‘Prelude’ at ...Development/IDE/LSP/CodeAction.hs:7:8-37
--          (and originally defined in ‘GHC.Base’)) (lsp-ui)

    | topOfHoleFitsMarker `T.isInfixOf` _message = let
      findSuggestedHoleFits :: T.Text -> [T.Text]
      findSuggestedHoleFits = extractFitNames . selectLinesWithFits . dropPreceding . T.lines
      proposeHoleFit name = ("replace hole `" <> holeName <>  "` with " <> name, [TextEdit _range name])
      holeName = T.strip $ last $ T.splitOn ":" $ head . T.splitOn "::" $ head $ filter ("Found hole" `T.isInfixOf`) $ T.lines _message
      dropPreceding       = dropWhile (not . (topOfHoleFitsMarker `T.isInfixOf`))
      selectLinesWithFits = filter ("::" `T.isInfixOf`)
      extractFitNames     = map (T.strip . head . T.splitOn " :: ")
      in map proposeHoleFit $ nubOrd $ findSuggestedHoleFits _message

    | otherwise = []

suggestExtendImport :: Maybe DynFlags -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestExtendImport (Just dflags) contents Diagnostic{_range=_range,..}
    | Just [binding, mod, srcspan] <-
      matchRegex _message
      "Perhaps you want to add ‘([^’]*)’ to the import list in the import of ‘([^’]*)’ *\\((.*)\\).$"
    , Just c <- contents
    , POk _ (L _ name) <- runParser dflags (T.unpack binding) parseIdentifier
    = let range = case [ x | (x,"") <- readSrcSpan (T.unpack srcspan)] of
            [s] -> let x = srcSpanToRange s
                   in x{_end = (_end x){_character = succ (_character (_end x))}}
            _ -> error "bug in srcspan parser"
          importLine = textInRange range c
        in [("Add " <> binding <> " to the import list of " <> mod
        , [TextEdit range (addBindingToImportList (T.pack $ printRdrName name) importLine)])]
    | otherwise = []
suggestExtendImport Nothing _ _ = []

suggestFixConstructorImport :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestFixConstructorImport _ Diagnostic{_range=_range,..}
    -- ‘Success’ is a data constructor of ‘Result’
    -- To import it use
    -- import Data.Aeson.Types( Result( Success ) )
    -- or
    -- import Data.Aeson.Types( Result(..) ) (lsp-ui)
  | Just [constructor, typ] <-
    matchRegex _message
    "‘([^’]*)’ is a data constructor of ‘([^’]*)’ To import it use"
  = let fixedImport = typ <> "(" <> constructor <> ")"
    in [("Fix import of " <> fixedImport, [TextEdit _range fixedImport])]
  | otherwise = []

suggestSignature :: Bool -> Diagnostic -> [(T.Text, [TextEdit])]
suggestSignature isQuickFix Diagnostic{_range=_range@Range{..},..}
    | _message =~
      ("(Top-level binding|Polymorphic local binding|Pattern synonym) with no type signature" :: T.Text) = let
      signature      = removeInitialForAll
                     $ T.takeWhile (\x -> x/='*' && x/='•')
                     $ T.strip $ unifySpaces $ last $ T.splitOn "type signature: " $ filterNewlines _message
      startOfLine    = Position (_line _start) startCharacter
      beforeLine     = Range startOfLine startOfLine
      title          = if isQuickFix then "add signature: " <> signature else signature
      action         = TextEdit beforeLine $ signature <> "\n" <> T.replicate startCharacter " "
      in [(title, [action])]
    where removeInitialForAll :: T.Text -> T.Text
          removeInitialForAll (T.breakOnEnd " :: " -> (nm, ty))
              | "forall" `T.isPrefixOf` ty = nm <> T.drop 2 (snd (T.breakOn "." ty))
              | otherwise                  = nm <> ty
          startCharacter
            | "Polymorphic local binding" `T.isPrefixOf` _message
            = _character _start
            | otherwise
            = 0

suggestSignature _ _ = []

-- | Suggests a constraint for a declaration for which a constraint is missing.
suggestConstraint :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestConstraint mContents diag@Diagnostic {..}
  | Just contents <- mContents
  , Just missingConstraint <- findMissingConstraint _message
  = let codeAction = if _message =~ ("the type signature for:" :: String)
                        then suggestFunctionConstraint
                        else suggestInstanceConstraint
     in codeAction contents diag missingConstraint
  | otherwise = []
    where
      findMissingConstraint :: T.Text -> Maybe T.Text
      findMissingConstraint t =
        let regex = "(No instance for|Could not deduce) \\((.+)\\) arising from a use of"
         in matchRegex t regex <&> last

normalizeConstraints :: T.Text -> T.Text -> T.Text
normalizeConstraints existingConstraints constraint =
  let constraintsInit = if "(" `T.isPrefixOf` existingConstraints
                           then T.dropEnd 1 existingConstraints
                           else "(" <> existingConstraints
   in constraintsInit <> ", " <> constraint <> ")"

-- | Suggests a constraint for an instance declaration for which a constraint is missing.
suggestInstanceConstraint :: T.Text -> Diagnostic -> T.Text -> [(T.Text, [TextEdit])]
suggestInstanceConstraint contents Diagnostic {..} missingConstraint
-- Suggests a constraint for an instance declaration with no existing constraints.
-- • No instance for (Eq a) arising from a use of ‘==’
--   Possible fix: add (Eq a) to the context of the instance declaration
-- • In the expression: x == y
--   In an equation for ‘==’: (Wrap x) == (Wrap y) = x == y
--   In the instance declaration for ‘Eq (Wrap a)’
  | Just [instanceDeclaration] <- matchRegex _message "In the instance declaration for ‘([^`]*)’"
  = let instanceLine = contents
          & T.splitOn ("instance " <> instanceDeclaration)
          & head & T.lines & length
        startOfConstraint = Position instanceLine (length ("instance " :: String))
        range = Range startOfConstraint startOfConstraint
        newConstraint = missingConstraint <> " => "
     in [(actionTitle missingConstraint, [TextEdit range newConstraint])]

-- Suggests a constraint for an instance declaration with one or more existing constraints.
-- • Could not deduce (Eq b) arising from a use of ‘==’
--   from the context: Eq a
--     bound by the instance declaration at /path/to/Main.hs:7:10-32
--   Possible fix: add (Eq b) to the context of the instance declaration
-- • In the second argument of ‘(&&)’, namely ‘x' == y'’
--   In the expression: x == y && x' == y'
--   In an equation for ‘==’:
--       (Pair x x') == (Pair y y') = x == y && x' == y'
  | Just [instanceLineStr, constraintFirstCharStr]
    <- matchRegex _message "bound by the instance declaration at .+:([0-9]+):([0-9]+)"
  = let existingConstraints = findExistingConstraints _message
        newConstraints = normalizeConstraints existingConstraints missingConstraint
        instanceLine = readPositionNumber instanceLineStr
        constraintFirstChar = readPositionNumber constraintFirstCharStr
        startOfConstraint = Position instanceLine constraintFirstChar
        endOfConstraint = Position instanceLine $
          constraintFirstChar + T.length existingConstraints
        range = Range startOfConstraint endOfConstraint
     in [(actionTitle missingConstraint, [TextEdit range newConstraints])]
  | otherwise = []
    where
      findExistingConstraints :: T.Text -> T.Text
      findExistingConstraints t =
        T.replace "from the context: " "" . T.strip $ T.lines t !! 1

      readPositionNumber :: T.Text -> Int
      readPositionNumber = T.unpack >>> read >>> pred

      actionTitle :: T.Text -> T.Text
      actionTitle constraint = "Add `" <> constraint
        <> "` to the context of the instance declaration"

findTypeSignatureName :: T.Text -> Maybe T.Text
findTypeSignatureName t = matchRegex t "([^ ]+) :: " <&> head

findTypeSignatureLine :: T.Text -> T.Text -> Int
findTypeSignatureLine contents typeSignatureName =
  T.splitOn (typeSignatureName <> " :: ") contents & head & T.lines & length

-- | Suggests a constraint for a type signature for which a constraint is missing.
suggestFunctionConstraint :: T.Text -> Diagnostic -> T.Text -> [(T.Text, [TextEdit])]
suggestFunctionConstraint contents Diagnostic{..} missingConstraint
-- Suggests a constraint for a type signature with any number of existing constraints.
-- • No instance for (Eq a) arising from a use of ‘==’
--   Possible fix:
--     add (Eq a) to the context of
--       the type signature for:
--         eq :: forall a. a -> a -> Bool
-- • In the expression: x == y
--   In an equation for ‘eq’: eq x y = x == y

-- • Could not deduce (Eq b) arising from a use of ‘==’
--   from the context: Eq a
--     bound by the type signature for:
--                eq :: forall a b. Eq a => Pair a b -> Pair a b -> Bool
--     at Main.hs:5:1-42
--   Possible fix:
--     add (Eq b) to the context of
--       the type signature for:
--         eq :: forall a b. Eq a => Pair a b -> Pair a b -> Bool
-- • In the second argument of ‘(&&)’, namely ‘y == y'’
--   In the expression: x == x' && y == y'
--   In an equation for ‘eq’:
--       eq (Pair x y) (Pair x' y') = x == x' && y == y'
  | Just typeSignatureName <- findTypeSignatureName _message
  = let mExistingConstraints = findExistingConstraints _message
        newConstraint = buildNewConstraints missingConstraint mExistingConstraints
        typeSignatureLine = findTypeSignatureLine contents typeSignatureName
        typeSignatureFirstChar = T.length $ typeSignatureName <> " :: "
        startOfConstraint = Position typeSignatureLine typeSignatureFirstChar
        endOfConstraint = Position typeSignatureLine $
          typeSignatureFirstChar + maybe 0 T.length mExistingConstraints
        range = Range startOfConstraint endOfConstraint
     in [(actionTitle missingConstraint typeSignatureName, [TextEdit range newConstraint])]
  | otherwise = []
    where
      findExistingConstraints :: T.Text -> Maybe T.Text
      findExistingConstraints message =
        if message =~ ("from the context:" :: String)
           then fmap (T.strip . head) $ matchRegex message "\\. ([^=]+)"
           else Nothing

      buildNewConstraints :: T.Text -> Maybe T.Text -> T.Text
      buildNewConstraints constraint mExistingConstraints =
        case mExistingConstraints of
          Just existingConstraints -> normalizeConstraints existingConstraints constraint
          Nothing -> constraint <> " => "

      actionTitle :: T.Text -> T.Text -> T.Text
      actionTitle constraint typeSignatureName = "Add `" <> constraint
        <> "` to the context of the type signature for `" <> typeSignatureName <> "`"

-------------------------------------------------------------------------------------------------

suggestNewImport :: PackageExportsMap -> ParsedModule -> Diagnostic -> [(T.Text, [TextEdit])]
suggestNewImport packageExportsMap ParsedModule {pm_parsed_source = L _ HsModule {..}} Diagnostic{_message}
  | msg <- unifySpaces _message
  , Just name <- extractNotInScopeName msg
  , Just insertLine <- case hsmodImports of
        [] -> case srcSpanStart $ getLoc (head hsmodDecls) of
          RealSrcLoc s -> Just $ srcLocLine s - 1
          _ -> Nothing
        _ -> case srcSpanEnd $ getLoc (last hsmodImports) of
          RealSrcLoc s -> Just $ srcLocLine s
          _ -> Nothing
  , insertPos <- Position insertLine 0
  , extendImportSuggestions <- matchRegex msg
    "Perhaps you want to add ‘[^’]*’ to the import list in the import of ‘([^’]*)’"
  = [(imp, [TextEdit (Range insertPos insertPos) (imp <> "\n")])
    | imp <- constructNewImportSuggestions packageExportsMap name extendImportSuggestions
    ]
suggestNewImport _ _ _ = []

constructNewImportSuggestions
  :: PackageExportsMap -> NotInScope -> Maybe [T.Text] -> [T.Text]
constructNewImportSuggestions exportsMap thingMissing notTheseModules = nubOrd
  [ renderNewImport identInfo m
  | (identInfo, m) <- fromMaybe [] $ Map.lookup name exportsMap
  , canUseIdent thingMissing identInfo
  , m `notElem` fromMaybe [] notTheseModules
  ]
 where
  renderNewImport identInfo m
    | Just q <- qual = "import qualified " <> m <> " as " <> q
    | otherwise      = "import " <> m <> " (" <> importWhat identInfo <> ")"

  (qual, name) = case T.splitOn "." (notInScope thingMissing) of
    [n]      -> (Nothing, n)
    segments -> (Just (T.concat $ init segments), last segments)
  importWhat IdentInfo {parent, rendered}
    | Just p <- parent = p <> "(" <> rendered <> ")"
    | otherwise        = rendered

canUseIdent :: NotInScope -> IdentInfo -> Bool
canUseIdent NotInScopeDataConstructor{} = isDatacon
canUseIdent _                           = const True

data NotInScope
    = NotInScopeDataConstructor T.Text
    | NotInScopeTypeConstructorOrClass T.Text
    | NotInScopeThing T.Text
    deriving Show

notInScope :: NotInScope -> T.Text
notInScope (NotInScopeDataConstructor t) = t
notInScope (NotInScopeTypeConstructorOrClass t) = t
notInScope (NotInScopeThing t) = t

extractNotInScopeName :: T.Text -> Maybe NotInScope
extractNotInScopeName x
  | Just [name] <- matchRegex x "Data constructor not in scope: ([^ ]+)"
  = Just $ NotInScopeDataConstructor name
  | Just [name] <- matchRegex x "Not in scope: data constructor [^‘]*‘([^’]*)’"
  = Just $ NotInScopeDataConstructor name
  | Just [name] <- matchRegex x "ot in scope: type constructor or class [^‘]*‘([^’]*)’"
  = Just $ NotInScopeTypeConstructorOrClass name
  | Just [name] <- matchRegex x "ot in scope: \\(([^‘ ]+)\\)"
  = Just $ NotInScopeThing name
  | Just [name] <- matchRegex x "ot in scope: ([^‘ ]+)"
  = Just $ NotInScopeThing name
  | Just [name] <- matchRegex x "ot in scope:[^‘]*‘([^’]*)’"
  = Just $ NotInScopeThing name
  | otherwise
  = Nothing

-------------------------------------------------------------------------------------------------

topOfHoleFitsMarker :: T.Text
topOfHoleFitsMarker =
#if MIN_GHC_API_VERSION(8,6,0)
  "Valid hole fits include"
#else
  "Valid substitutions include"
#endif

mkRenameEdit :: Maybe T.Text -> Range -> T.Text -> TextEdit
mkRenameEdit contents range name =
    if maybeIsInfixFunction == Just True
      then TextEdit range ("`" <> name <> "`")
      else TextEdit range name
  where
    maybeIsInfixFunction = do
      curr <- textInRange range <$> contents
      pure $ "`" `T.isPrefixOf` curr && "`" `T.isSuffixOf` curr

extractWildCardTypeSignature :: T.Text -> T.Text
extractWildCardTypeSignature =
  -- inferring when parens are actually needed around the type signature would
  -- require understanding both the precedence of the context of the _ and of
  -- the signature itself. Inserting them unconditionally is ugly but safe.
  ("(" `T.append`) . (`T.append` ")") .
  T.takeWhile (/='’') . T.dropWhile (=='‘') . T.dropWhile (/='‘') .
  snd . T.breakOnEnd "standing for "

extractRenamableTerms :: T.Text -> [T.Text]
extractRenamableTerms msg
  -- Account for both "Variable not in scope" and "Not in scope"
  | "ot in scope:" `T.isInfixOf` msg = extractSuggestions msg
  | otherwise = []
  where
    extractSuggestions = map getEnclosed
                       . concatMap singleSuggestions
                       . filter isKnownSymbol
                       . T.lines
    singleSuggestions = T.splitOn "), " -- Each suggestion is comma delimited
    isKnownSymbol t = " (imported from" `T.isInfixOf` t || " (line " `T.isInfixOf` t
    getEnclosed = T.dropWhile (== '‘')
                . T.dropWhileEnd (== '’')
                . T.dropAround (\c -> c /= '‘' && c /= '’')

-- | If a range takes up a whole line (it begins at the start of the line and there's only whitespace
-- between the end of the range and the next newline), extend the range to take up the whole line.
extendToWholeLineIfPossible :: Maybe T.Text -> Range -> Range
extendToWholeLineIfPossible contents range@Range{..} =
    let newlineAfter = maybe False (T.isPrefixOf "\n" . T.dropWhile (\x -> isSpace x && x /= '\n') . snd . splitTextAtPosition _end) contents
        extend = newlineAfter && _character _start == 0 -- takes up an entire line, so remove the whole line
    in if extend then Range _start (Position (_line _end + 1) 0) else range

splitTextAtPosition :: Position -> T.Text -> (T.Text, T.Text)
splitTextAtPosition (Position row col) x
    | (preRow, mid:postRow) <- splitAt row $ T.splitOn "\n" x
    , (preCol, postCol) <- T.splitAt col mid
        = (T.intercalate "\n" $ preRow ++ [preCol], T.intercalate "\n" $ postCol : postRow)
    | otherwise = (x, T.empty)

-- | Returns [start .. end[
textInRange :: Range -> T.Text -> T.Text
textInRange (Range (Position startRow startCol) (Position endRow endCol)) text =
    case compare startRow endRow of
      LT ->
        let (linesInRangeBeforeEndLine, endLineAndFurtherLines) = splitAt (endRow - startRow) linesBeginningWithStartLine
            (textInRangeInFirstLine, linesBetween) = case linesInRangeBeforeEndLine of
              [] -> ("", [])
              firstLine:linesInBetween -> (T.drop startCol firstLine, linesInBetween)
            maybeTextInRangeInEndLine = T.take endCol <$> listToMaybe endLineAndFurtherLines
        in T.intercalate "\n" (textInRangeInFirstLine : linesBetween ++ maybeToList maybeTextInRangeInEndLine)
      EQ ->
        let line = fromMaybe "" (listToMaybe linesBeginningWithStartLine)
        in T.take (endCol - startCol) (T.drop startCol line)
      GT -> ""
    where
      linesBeginningWithStartLine = drop startRow (T.splitOn "\n" text)

-- | Returns the ranges for a binding in an import declaration
rangesForBinding :: ImportDecl GhcPs -> String -> [Range]
rangesForBinding ImportDecl{ideclHiding = Just (False, L _ lies)} b =
    concatMap (map srcSpanToRange . rangesForBinding' b') lies
  where
    b' = wrapOperatorInParens (unqualify b)

    wrapOperatorInParens x = if isAlpha (head x) then x else "(" <> x <> ")"

    unqualify x = snd $ breakOnEnd "." x

rangesForBinding _ _ = []

rangesForBinding' :: String -> LIE GhcPs -> [SrcSpan]
rangesForBinding' b (L l x@IEVar{}) | showSDocUnsafe (ppr x) == b = [l]
rangesForBinding' b (L l x@IEThingAbs{}) | showSDocUnsafe (ppr x) == b = [l]
rangesForBinding' b (L l (IEThingAll x)) | showSDocUnsafe (ppr x) == b = [l]
rangesForBinding' b (L l (IEThingWith thing _  inners labels))
    | showSDocUnsafe (ppr thing) == b = [l]
    | otherwise =
        [ l' | L l' x <- inners, showSDocUnsafe (ppr x) == b] ++
        [ l' | L l' x <- labels, showSDocUnsafe (ppr x) == b]
rangesForBinding' _ _ = []

-- | Extends an import list with a new binding.
--   Assumes an import statement of the form:
--       import (qualified) A (..) ..
--   Places the new binding first, preserving whitespace.
--   Copes with multi-line import lists
addBindingToImportList :: T.Text -> T.Text -> T.Text
addBindingToImportList binding importLine = case T.breakOn "(" importLine of
    (pre, T.uncons -> Just (_, rest)) ->
      case T.uncons (T.dropWhile isSpace rest) of
        Just (')', _) -> T.concat [pre, "(", binding, rest]
        _             -> T.concat [pre, "(", binding, ", ", rest]
    _ ->
      error
        $  "importLine does not have the expected structure: "
        <> T.unpack importLine

-- | Returns Just (the submatches) for the first capture, or Nothing.
matchRegex :: T.Text -> T.Text -> Maybe [T.Text]
matchRegex message regex = case unifySpaces message =~~ regex of
    Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, bindings) -> Just bindings
    Nothing -> Nothing

setHandlersCodeLens :: PartialHandlers c
setHandlersCodeLens = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.codeLensHandler = withResponse RspCodeLens codeLens,
    LSP.executeCommandHandler = withResponseAndRequest RspExecuteCommand ReqApplyWorkspaceEdit executeAddSignatureCommand
    }

filterNewlines :: T.Text -> T.Text
filterNewlines = T.concat  . T.lines

unifySpaces :: T.Text -> T.Text
unifySpaces    = T.unwords . T.words
