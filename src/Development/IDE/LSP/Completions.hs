module Development.IDE.LSP.Completions (
  setHandlersCompletion
) where

import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.VFS as VFS
import Language.Haskell.LSP.Types.Capabilities

import Development.IDE.Core.Service
import Development.IDE.Core.Completions
import Development.IDE.Types.Location
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Development.IDE.LSP.Server

-- | Generate code actions.
getCompletionsLSP
    :: LSP.LspFuncs ()
    -> IdeState
    -> CompletionParams
    -> IO CompletionResponseResult
getCompletionsLSP lsp ide CompletionParams{_textDocument=TextDocumentIdentifier uri,_position=position} = do
    -- disable logging as its quite verbose
    -- logInfo (ideLogger ide) $ T.pack $ "Code action req: " ++ show arg
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    case (contents, uriToFilePath' uri) of
      (Just cnts, Just path) -> do
        pfix <- VFS.getCompletionPrefix position cnts
        let npath = toNormalizedFilePath path
        (tm, cci) <- runAction ide ( (,) <$> use TypeCheck npath <*> use ProduceCompletions npath )
        case (pfix, tm, cci) of
          (Just pfix', Just tm', Just cci') -> do
            let fakeClientCapabilities = ClientCapabilities Nothing Nothing Nothing Nothing
            Completions . List <$> getCompletions cci' (tmrModule tm') pfix' fakeClientCapabilities (WithSnippets True)
          _ -> return (Completions $ List [])
      _ -> return (Completions $ List [])

setHandlersCompletion :: PartialHandlers
setHandlersCompletion = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.completionHandler = withResponse RspCompletion getCompletionsLSP
    }
