{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Development.IDE.Plugin where

import Data.Default
import qualified Data.Text as T
import Development.Shake

import           Language.LSP.Types
import Development.IDE.Compat
import Development.IDE.Core.Rules
import Development.IDE.LSP.Server
import qualified Language.LSP.Core as LSP

data Plugin c = Plugin
    {pluginRules :: Rules ()
    ,pluginHandlers :: LSP.Handlers (ServerM c)
    }

instance Default (Plugin c) where
    def = Plugin mempty mempty

instance Semigroup (Plugin c) where
    Plugin x1 h1 <> Plugin x2 h2 = Plugin (x1<>x2) (h1 <> h2)

instance Monoid (Plugin c) where
    mempty = def


codeActionPlugin :: (IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> LSP.LspM c (Either ResponseError [Command |? CodeAction])) -> Plugin c
codeActionPlugin = codeActionPluginWithRules mempty

codeActionPluginWithRules :: forall c. Rules () -> (IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> LSP.LspM c (Either ResponseError [Command |? CodeAction])) -> Plugin c
codeActionPluginWithRules rr f = Plugin rr handlers
  where
    handlers :: LSP.Handlers (ServerM c)
    handlers = requestHandler STextDocumentCodeAction g
    g state (CodeActionParams _ _ a b c) = fmap List <$> f state a b c

-- | Prefix to uniquely identify commands sent to the client.  This
-- has two parts
--
-- - A representation of the process id to make sure that a client has
--   unique commands if it is running multiple servers, since some
--   clients have a global command table and get confused otherwise.
--
-- - A string to identify ghcide, to ease integration into
--   haskell-language-server, which routes commands to plugins based
--   on that.
makeLspCommandId :: T.Text -> IO T.Text
makeLspCommandId command = do
    pid <- getPid
    return $ pid <> ":ghcide:" <> command

-- | Get the operating system process id for the running server
-- instance. This should be the same for the lifetime of the instance,
-- and different from that of any other currently running instance.
getPid :: IO T.Text
getPid = T.pack . show <$> getProcessID
