-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.GHC.Warnings(withWarnings) where

import ErrUtils
import GhcPlugins as GHC hiding (Var, (<>))

import           Control.Concurrent.Extra
import qualified           Data.Text as T

import           Development.IDE.Types.Diagnostics
import           Development.IDE.GHC.Error
import           Language.Haskell.LSP.Types (NumberOrString (StringValue))


-- | Take a GHC monadic action (e.g. @typecheckModule pm@ for some
-- parsed module 'pm@') and produce a "decorated" action that will
-- harvest any warnings encountered executing the action. The 'phase'
-- argument classifies the context (e.g. "Parser", "Typechecker").
--
--   The ModSummary function is required because of
--   https://github.com/ghc/ghc/blob/5f1d949ab9e09b8d95319633854b7959df06eb58/compiler/main/GHC.hs#L623-L640
--   which basically says that log_action is taken from the ModSummary when GHC feels like it.
--   The given argument lets you refresh a ModSummary log_action
withWarnings :: T.Text -> ((ModSummary -> ModSummary) -> IO a) -> IO ([(WarnReason, FileDiagnostic)], a)
withWarnings diagSource action = do
  warnings <- newVar []
  let newAction :: DynFlags -> WarnReason -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
      newAction dynFlags wr _ loc style msg = do
        let wr_d = map ((wr,) . third3 (attachReason wr)) $ diagFromErrMsg diagSource dynFlags $ mkWarnMsg dynFlags loc (queryQual style) msg
        modifyVar_ warnings $ return . (wr_d:)
  res <- action $ \x -> x{ms_hspp_opts = (ms_hspp_opts x){log_action = newAction}}
  warns <- readVar warnings
  return (reverse $ concat warns, res)

attachReason :: WarnReason -> Diagnostic -> Diagnostic
attachReason wr d = d{_code = StringValue <$> showReason wr}
  where
    showReason = \case
        NoReason -> Nothing
        Reason flag -> Just $ showFlag flag
        ErrReason flag -> showFlag <$> flag

showFlag :: WarningFlag -> T.Text
showFlag f = "-W" <> case f of
    Opt_WarnAllMissedSpecs -> "all-missed-specialisations"
    Opt_WarnAlternativeLayoutRuleTransitional -> "alternative-layout-rule-transitional"
    Opt_WarnAutoOrphans -> "auto-orphans"
    Opt_WarnCPPUndef -> "cpp-undef"
    Opt_WarnDeferredOutOfScopeVariables -> "deferred-out-of-scope-variables"
    Opt_WarnDeferredTypeErrors -> "deferred-type-errors"
    Opt_WarnDeprecatedFlags -> "deprecated-flags"
    Opt_WarnDerivingTypeable -> "deriving-typeable"
    Opt_WarnDodgyExports -> "dodgy-exports"
    Opt_WarnDodgyForeignImports -> "dodgy-foreign-imports"
    Opt_WarnDodgyImports -> "dodgy-imports"
    Opt_WarnDuplicateConstraints -> "duplicate-constraints"
    Opt_WarnDuplicateExports -> "duplicate-exports"
    Opt_WarnEmptyEnumerations -> "empty-enumerations"
    Opt_WarnHiShadows -> "hi-shadowing"
    Opt_WarnIdentities -> "identities"
    Opt_WarnImplicitKindVars -> "implicit-kind-vars"
    Opt_WarnImplicitPrelude -> "implicit-prelude"
    Opt_WarnInaccessibleCode -> "inaccessible-code"
    Opt_WarnIncompletePatterns -> "incomplete-patterns"
    Opt_WarnIncompletePatternsRecUpd -> "incomplete-record-updates"
    Opt_WarnIncompleteUniPatterns -> "incomplete-uni-patterns"
    Opt_WarnInlineRuleShadowing -> "inline-rule-shadowing"
    Opt_WarnMissedExtraSharedLib -> "missed-extra-shared-lib"
    Opt_WarnMissedSpecs -> "missed-specializations"
    Opt_WarnMissingDerivingStrategies -> "missing-deriving-strategies"
    Opt_WarnMissingExportedSignatures -> "missing-export-lists"
    Opt_WarnMissingExportList -> "missing-exported-signatures"
    Opt_WarnMissingFields -> "missing-fields"
    Opt_WarnMissingHomeModules -> "missing-home-modules"
    Opt_WarnMissingImportList -> "missing-import-lists"
    Opt_WarnMissingLocalSignatures -> "missing-local-signatures"
    Opt_WarnMissingMethods -> "missing-methods"
    Opt_WarnMissingMonadFailInstances -> "missing-monadfail-instances"
    Opt_WarnMissingPatternSynonymSignatures -> "missing-pattern-synonym-signatures"
    Opt_WarnMissingSignatures -> "missing-signatures"
    Opt_WarnMonomorphism -> "monomorphism-restriction"
    Opt_WarnNameShadowing -> "name-shadowing"
    Opt_WarnNonCanonicalMonadFailInstances -> "noncanonical-monadfail-instances"
    Opt_WarnNonCanonicalMonadInstances -> "noncanonical-monad-instances"
    Opt_WarnNonCanonicalMonoidInstances -> "noncanonical-monoid-instances"
    Opt_WarnOrphans -> "orphans"
    Opt_WarnOverflowedLiterals -> "overflowed-literals"
    Opt_WarnOverlappingPatterns -> "overlapping-patterns"
    Opt_WarnPartialFields -> "partial-fields"
    Opt_WarnPartialTypeSignatures -> "partial-type-signatures"
    Opt_WarnRedundantConstraints -> "redundant-constraints"
    Opt_WarnSafe -> "safe"
    Opt_WarnSemigroup -> "semigroup"
    Opt_WarnSimplifiableClassConstraints -> "simplifiable-class-constraints"
    Opt_WarnSpaceAfterBang -> "missing-space-after-bang"
    Opt_WarnStarBinder -> "star-binder"
    Opt_WarnStarIsType -> "star-is-type"
    Opt_WarnTabs -> "tabs"
    Opt_WarnTrustworthySafe -> "trustworthy-safe"
    Opt_WarnTypeDefaults -> "type-defaults"
    Opt_WarnTypedHoles -> "typed-holes"
    Opt_WarnUnbangedStrictPatterns -> "unbanged-strict-patterns"
    Opt_WarnUnrecognisedPragmas -> "unrecognised-pragmas"
    Opt_WarnUnrecognisedWarningFlags -> "unrecognisedarning-flags"
    Opt_WarnUnsafe -> "unsafe"
    Opt_WarnUnsupportedCallingConventions -> "unsupported-calling-conventions"
    Opt_WarnUnsupportedLlvmVersion -> "unsupported-llvm-version"
    Opt_WarnUntickedPromotedConstructors -> "unticked-promoted-constructors"
    Opt_WarnUnusedDoBind -> "unused-do-bind"
    Opt_WarnUnusedForalls -> "unused-foralls"
    Opt_WarnUnusedImports -> "unused-imports"
    Opt_WarnUnusedLocalBinds -> "unused-local-binds"
    Opt_WarnUnusedMatches -> "unused-matches"
    Opt_WarnUnusedPatternBinds -> "unused-pattern-binds"
    Opt_WarnUnusedTopBinds -> "unused-top-binds"
    Opt_WarnUnusedTypePatterns -> "unused-type-patterns"
    Opt_WarnWarningsDeprecations ->  "deprecations"
    Opt_WarnWrongDoBind -> "wrong-do-bind"
