module Development.IDE.Plugin.CodeAction.Rules
  ( rulePackageExports,
    getVisiblePackageExports
  )
where

import Data.HashMap.Strict (fromListWith)
import qualified Data.HashMap.Strict as Map
import Data.Text ( Text, pack,)
import Data.Traversable (forM)
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Development.IDE.GHC.Util
import Development.IDE.Plugin.CodeAction.RuleTypes
import Development.IDE.Types.Location
import Development.Shake
import HscTypes (hsc_dflags, IfaceExport, mi_exports)
import LoadIface
import Maybes
import Module (Module (..), ModuleName, moduleNameString)
import Packages (explicitPackages, exposedModules, packageConfigId)
import TcRnMonad (WhereFrom (ImportByUser), initIfaceLoad)
import GHC (DynFlags(pkgState))
import Control.Monad.Trans (MonadTrans(lift))

getVisiblePackageExports :: NormalizedFilePath -> Action [PackageExportsMap]
getVisiblePackageExports f = fmap (fromMaybe []) $ runMaybeT $ do
        session <- MaybeT $ use GhcSession f
        let exposedPkgs = explicitPackages $ pkgState (hsc_dflags $ hscEnv session)
        traverse (\p -> lift $ use_ (fromUnitId p) f) exposedPkgs

rulePackageExports :: Rules ()
rulePackageExports = define $ \p file -> do
  pkgState <- hscEnv <$> use_ GhcSession file
  let pkg = lookupPackageConfig (packageExportsUnitId p) pkgState
  case pkg of
    Nothing -> return ([], Just mempty)
    Just pkg -> do
      results <- forM (exposedModules pkg) $ \(mn, _) -> do
            modIface <-
              liftIO $ initIfaceLoad pkgState $
                loadInterface
                  ""
                  (Module (packageConfigId pkg) mn)
                  (ImportByUser False)
            case modIface of
              Failed _err -> return mempty
              Succeeded mi -> do
                let avails = mi_exports mi
                return $ fromListWith (++) $
                  concatMap
                    (unpackAvail mn)
                    avails
      return ([], Just $ foldr (Map.unionWith (++)) Map.empty results)

unpackAvail :: ModuleName -> IfaceExport -> [(Text, [(IdentInfo, Text)])]
unpackAvail mod =
  map (\id@IdentInfo {..} -> (name, [(id, pack $ moduleNameString mod)]))
    . mkIdentInfos
