{-# LANGUAGE GADTs #-}

module Development.IDE.Plugin
    (
      Plugin(..)
    , CommandId(..)
    , PluginCommand(..)
    , codeActionPlugin
    , codeActionPluginWithRules
    ) where

import Data.Default
import Development.Shake
import Development.IDE.LSP.Server

import           Data.Aeson
import           Data.String
import qualified Data.Text                 as T
import           Language.Haskell.LSP.Types
import Development.IDE.Core.Rules
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Messages



data Plugin c = Plugin
    {pluginCommands :: ![PluginCommand]
    ,pluginRules :: Rules ()
    ,pluginHandler :: PartialHandlers c
    }

instance Default (Plugin c) where
    def = Plugin mempty mempty def

instance Semigroup (Plugin c) where
    Plugin x1 y1 z1 <> Plugin x2 y2 z2 = Plugin (x1<>x2) (y1<>y2) (z1<>z2)

instance Monoid (Plugin c) where
    mempty = def

newtype CommandId = CommandId T.Text
  deriving (Show, Read, Eq, Ord)
instance IsString CommandId where
  fromString = CommandId . T.pack

data PluginCommand = forall a. (FromJSON a) =>
  PluginCommand { commandId   :: CommandId
                , commandDesc :: T.Text
                , commandFunc :: a -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
                }


codeActionPlugin :: (LSP.LspFuncs c -> IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> IO (Either ResponseError [CAResult])) -> Plugin c
codeActionPlugin = codeActionPluginWithRules mempty

codeActionPluginWithRules :: Rules () -> (LSP.LspFuncs c -> IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> IO (Either ResponseError [CAResult])) -> Plugin c
codeActionPluginWithRules rr f = Plugin mempty rr $ PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.codeActionHandler = withResponse RspCodeAction g
    }
    where
      g lsp state (CodeActionParams a b c _) = fmap List <$> f lsp state a b c
