module Korektor.Lsp (runLsp) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Korektor.Cli (EnvCache, Settings (..), envFor, lintSource, newEnvCache, settingsFor)
import Korektor.Config (TargetSpec (..), emptyConfig, toTarget)
import Korektor.Position
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types qualified as J
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText, virtualFileVersion)
import Rozbir.Edit (TextEdit (..))
import Rozbir.Eval (Value, describeReason, expandText, reasons, runEval, showValue)
import Rozbir.Eval qualified as E
import Rozbir.Eval.Builtin (builtinNames, preludeMacros)
import Rozbir.IO (resolveIncludes)
import Rozbir.Lint qualified as Lint
import Rozbir.Query qualified as Q
import Rozbir.Spec hiding (defaultOptions)
import Rozbir.Syntax
import Rozbir.Syntax.Parse (parse)
import Rozbir.Syntax.Render (lineFrags, macroSpans, renderFrags, renderMacro)
import Rozbir.Version (renderEVR)
import System.Exit (ExitCode (..))

runLsp :: IO ExitCode
runLsp = do
  cache <- newEnvCache
  code <-
    runServer
      ServerDefinition
        { parseConfig = const (const (Right ())),
          onConfigChange = const (pure ()),
          defaultConfig = (),
          configSection = "korektor",
          doInitialize = \env _ -> pure (Right env),
          staticHandlers = const (handlers cache),
          interpretHandler = \env -> Iso (runLspT env) liftIO,
          options =
            defaultOptions
              { optTextDocumentSync =
                  Just (J.TextDocumentSyncOptions (Just True) (Just J.TextDocumentSyncKind_Full) Nothing Nothing (Just (J.InR (J.SaveOptions (Just False))))),
                optCompletionTriggerCharacters = Just ['%', '{'],
                optCodeActionKinds = Just [J.CodeActionKind_QuickFix],
                optServerInfo = Just (J.ServerInfo "korektor" Nothing)
              }
        }
  pure (if code == 0 then ExitSuccess else ExitFailure code)

data Doc = Doc
  { docUri :: J.Uri,
    docPath :: FilePath,
    docText :: Text,
    docVersion :: Maybe J.Int32,
    docIndex :: LineIndex
  }

loadDoc :: J.Uri -> LspM () (Maybe Doc)
loadDoc uri = do
  vf <- getVirtualFile (J.toNormalizedUri uri)
  pure $ do
    f <- vf
    let path = fromMaybe "untitled.spec" (J.uriToFilePath uri)
        txt = virtualFileText f
    Just (Doc uri path txt (Just (virtualFileVersion f)) (mkIndex txt))

settingsOf :: Doc -> IO (Maybe Settings)
settingsOf doc = either (const Nothing) Just <$> settingsFor Nothing emptyConfig (docPath doc)

specsOf :: EnvCache -> Doc -> IO (Tree, [(TargetSpec, Spec)])
specsOf cache doc = do
  let tree = parse (docText doc)
  includes <- resolveIncludes (docPath doc) tree
  settings <- settingsOf doc
  let st = fromMaybe (Settings Lint.defaultConfig [TargetSpec "unknown" Nothing []] False Lint.Warn) settings
  specs <- mapM (\t -> do env <- envFor cache (stSystemMacros st) t; pure (t, resolveWith (ResolveOptions env (toTarget t) includes) tree)) (stTargets st)
  pure (tree, specs)

toRange :: Doc -> Span -> J.Range
toRange doc (Span s e) =
  let (sl, sc) = toUtf16 (docIndex doc) s
      (el, ec) = toUtf16 (docIndex doc) e
   in J.Range (J.Position (fromIntegral sl) (fromIntegral sc)) (J.Position (fromIntegral el) (fromIntegral ec))

fromPosition :: Doc -> J.Position -> Pos
fromPosition doc (J.Position l c) = fromUtf16 (docIndex doc) (fromIntegral l) (fromIntegral c)

lspSeverity :: Lint.Severity -> J.DiagnosticSeverity
lspSeverity = \case
  Lint.Error -> J.DiagnosticSeverity_Error
  Lint.Warn -> J.DiagnosticSeverity_Warning
  Lint.Info -> J.DiagnosticSeverity_Information

toLspDiagnostic :: Doc -> Lint.Diagnostic -> J.Diagnostic
toLspDiagnostic doc d =
  J.Diagnostic
    { J._range = toRange doc (Lint.diagSpan d),
      J._severity = Just (lspSeverity (Lint.diagSeverity d)),
      J._code = Just (J.InR (Lint.diagRule d)),
      J._codeDescription = Nothing,
      J._source = Just "korektor",
      J._message = Lint.diagMessage d,
      J._tags = if Lint.diagRule d == "unused-ignore" then Just [J.DiagnosticTag_Unnecessary] else Nothing,
      J._relatedInformation = Nothing,
      J._data_ = Nothing
    }

diagnose :: EnvCache -> Doc -> IO [Lint.Diagnostic]
diagnose cache doc =
  settingsOf doc >>= \case
    Just st -> lintSource cache st (docPath doc) (docText doc)
    Nothing -> lintSource cache (Settings Lint.defaultConfig [TargetSpec "unknown" Nothing []] False Lint.Warn) (docPath doc) (docText doc)

publish :: EnvCache -> J.Uri -> LspM () ()
publish cache uri =
  loadDoc uri >>= \case
    Nothing -> pure ()
    Just doc -> do
      diags <- liftIO (diagnose cache doc)
      publishDiagnostics 200 (J.toNormalizedUri uri) (docVersion doc) (partitionBySource (map (toLspDiagnostic doc) diags))

handlers :: EnvCache -> Handlers (LspM ())
handlers cache =
  mconcat
    [ notificationHandler SMethod_Initialized (const (pure ())),
      notificationHandler SMethod_TextDocumentDidOpen $ \msg -> publish cache (msg ^. L.params . L.textDocument . L.uri),
      notificationHandler SMethod_TextDocumentDidChange $ \msg -> publish cache (msg ^. L.params . L.textDocument . L.uri),
      notificationHandler SMethod_TextDocumentDidSave $ \msg -> publish cache (msg ^. L.params . L.textDocument . L.uri),
      notificationHandler SMethod_TextDocumentDidClose (const (pure ())),
      notificationHandler SMethod_WorkspaceDidChangeConfiguration (const (pure ())),
      notificationHandler SMethod_SetTrace (const (pure ())),
      requestHandler SMethod_TextDocumentCodeAction $ \req respond -> do
        let params = req ^. L.params
            uri = params ^. L.textDocument . L.uri
        withDoc uri (respond (Right (J.InL []))) $ \doc -> do
          diags <- liftIO (diagnose cache doc)
          let wanted = params ^. L.range
              hits = [d | d <- diags, overlapsRange (toRange doc (Lint.diagSpan d)) wanted]
          respond (Right (J.InL (map J.InR (concatMap (codeActions doc) hits)))),
      requestHandler SMethod_TextDocumentHover $ \req respond -> do
        let params = req ^. L.params
        withDoc (params ^. L.textDocument . L.uri) (respond (Right (J.InR J.Null))) $ \doc -> do
          (tree, specs) <- liftIO (specsOf cache doc)
          let pos = fromPosition doc (params ^. L.position)
          respond . Right $ case hover tree specs pos of
            Just (sp, md) -> J.InL (J.Hover (J.InL (J.mkMarkdown md)) (Just (toRange doc sp)))
            Nothing -> J.InR J.Null,
      requestHandler SMethod_TextDocumentDefinition $ \req respond -> do
        let params = req ^. L.params
            uri = params ^. L.textDocument . L.uri
        withDoc uri (respond (Right (J.InR (J.InR J.Null)))) $ \doc -> do
          let tree = parse (docText doc)
              pos = fromPosition doc (params ^. L.position)
              locs = [J.Location uri (toRange doc sp) | sp <- definitionSpans tree pos]
          respond (Right (J.InL (J.Definition (J.InR locs)))),
      requestHandler SMethod_TextDocumentDocumentSymbol $ \req respond -> do
        let uri = req ^. L.params . L.textDocument . L.uri
        withDoc uri (respond (Right (J.InR (J.InL [])))) $ \doc ->
          respond (Right (J.InR (J.InL (symbols doc (parse (docText doc)))))),
      requestHandler SMethod_TextDocumentCompletion $ \req respond -> do
        let params = req ^. L.params
        withDoc (params ^. L.textDocument . L.uri) (respond (Right (J.InL []))) $ \doc -> do
          (tree, specs) <- liftIO (specsOf cache doc)
          respond (Right (J.InL (completions doc tree specs (params ^. L.position)))),
      requestHandler SMethod_TextDocumentReferences $ \req respond -> do
        let params = req ^. L.params
            uri = params ^. L.textDocument . L.uri
        withDoc uri (respond (Right (J.InL []))) $ \doc -> do
          let tree = parse (docText doc)
          respond (Right (J.InL [J.Location uri (toRange doc sp) | sp <- occurrences tree (fromPosition doc (params ^. L.position))])),
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req respond -> do
        let params = req ^. L.params
        withDoc (params ^. L.textDocument . L.uri) (respond (Right (J.InL []))) $ \doc -> do
          let tree = parse (docText doc)
          respond (Right (J.InL [J.DocumentHighlight (toRange doc sp) (Just J.DocumentHighlightKind_Text) | sp <- occurrences tree (fromPosition doc (params ^. L.position))])),
      requestHandler SMethod_TextDocumentPrepareRename $ \req respond -> do
        let params = req ^. L.params
        withDoc (params ^. L.textDocument . L.uri) (respond (Right (J.InR J.Null))) $ \doc -> do
          let tree = parse (docText doc)
          respond . Right $ case renameTarget tree (fromPosition doc (params ^. L.position)) of
            Just (sp, _) -> J.InL (J.PrepareRenameResult (J.InL (toRange doc sp)))
            Nothing -> J.InR J.Null,
      requestHandler SMethod_TextDocumentRename $ \req respond -> do
        let params = req ^. L.params
            uri = params ^. L.textDocument . L.uri
            newName = params ^. L.newName
        withDoc uri (respond (Right (J.InR J.Null))) $ \doc -> do
          let tree = parse (docText doc)
          case renameTarget tree (fromPosition doc (params ^. L.position)) of
            Just (_, oldName)
              | T.all isNameChar' newName && not (T.null newName) ->
                  respond (Right (J.InL (J.WorkspaceEdit (Just (Map.singleton uri [J.TextEdit (toRange doc sp) newName | sp <- nameSpans tree oldName])) Nothing Nothing)))
            _ -> respond (Right (J.InR J.Null)),
      requestHandler SMethod_TextDocumentInlayHint $ \req respond -> do
        let uri = req ^. L.params . L.textDocument . L.uri
        withDoc uri (respond (Right (J.InL []))) $ \doc -> do
          (tree, specs) <- liftIO (specsOf cache doc)
          respond (Right (J.InL (inlayHints doc tree specs))),
      requestHandler SMethod_TextDocumentCodeLens $ \req respond -> do
        let uri = req ^. L.params . L.textDocument . L.uri
        withDoc uri (respond (Right (J.InL []))) $ \doc -> do
          (tree, specs) <- liftIO (specsOf cache doc)
          respond (Right (J.InL (evrLenses doc tree specs))),
      requestHandler SMethod_TextDocumentSemanticTokensFull $ \req respond -> do
        let uri = req ^. L.params . L.textDocument . L.uri
        withDoc uri (respond (Right (J.InR J.Null))) $ \doc ->
          respond . Right $ case J.makeSemanticTokens J.defaultSemanticTokensLegend (semanticTokens doc (parse (docText doc))) of
            Right toks -> J.InL toks
            Left _ -> J.InR J.Null
    ]
  where
    withDoc uri missing act =
      loadDoc uri >>= \case
        Nothing -> missing
        Just doc -> act doc

overlapsRange :: J.Range -> J.Range -> Bool
overlapsRange (J.Range s1 e1) (J.Range s2 e2) = not (e1 `before` s2 || e2 `before` s1)
  where
    before (J.Position l1 c1) (J.Position l2 c2) = l1 < l2 || (l1 == l2 && c1 < c2)

codeActions :: Doc -> Lint.Diagnostic -> [J.CodeAction]
codeActions doc d = map fixAction (Lint.diagFixes d) <> [ignoreAction]
  where
    lspDiag = toLspDiagnostic doc d
    edit edits = J.WorkspaceEdit (Just (Map.singleton (docUri doc) edits)) Nothing Nothing
    fixAction f =
      J.CodeAction
        { J._title = Lint.fixTitle f,
          J._kind = Just J.CodeActionKind_QuickFix,
          J._diagnostics = Just [lspDiag],
          J._isPreferred = Just (Lint.fixSafe f),
          J._disabled = Nothing,
          J._edit = Just (edit [J.TextEdit (toRange doc sp) t | TextEdit sp t <- Lint.fixEdits f]),
          J._command = Nothing,
          J._data_ = Nothing
        }
    ignoreAction =
      let Span s _ = Lint.diagSpan d
          lineStart = Pos (posOffset s - posCol s) (posLine s) 0
          indent = maybe "" (T.takeWhile (\c -> c == ' ' || c == '\t') . snd) (lineAt (docIndex doc) (posLine s))
       in J.CodeAction
            { J._title = "ignore " <> Lint.diagRule d <> " on this line",
              J._kind = Just J.CodeActionKind_QuickFix,
              J._diagnostics = Just [lspDiag],
              J._isPreferred = Just False,
              J._disabled = Nothing,
              J._edit = Just (edit [J.TextEdit (toRange doc (Span lineStart lineStart)) (indent <> "# rozbir: ignore[" <> Lint.diagRule d <> "]\n")]),
              J._command = Nothing,
              J._data_ = Nothing
            }

macroAt :: Tree -> Pos -> Maybe (Line, Span, MacroRef)
macroAt tree pos =
  listToMaybe . sortOn (\(_, Span s e, _) -> posOffset e - posOffset s) $
    [ (l, sp, m)
    | l <- treeLines tree,
      spanContains (lineSpan l) pos,
      (p, fs) <- lineFrags l,
      (sp, m) <- macroSpans p fs,
      spanContains sp pos
    ]

lineAtPos :: Tree -> Pos -> Maybe Line
lineAtPos tree pos = listToMaybe [l | l <- treeLines tree, spanContains (lineSpan l) pos]

describeValue :: Value -> Text
describeValue v = case E.knownText v of
  Just t | T.null t -> "*(empty)*"
  Just t -> "`" <> T.replace "`" "'" t <> "`"
  Nothing -> "`" <> showValue v <> "` (" <> T.intercalate ", " (map describeReason (reasons v)) <> ")"

hover :: Tree -> [(TargetSpec, Spec)] -> Pos -> Maybe (Span, Text)
hover tree specs pos = case macroAt tree pos of
  Just (_, sp, m) ->
    let name = fromMaybe "" (macroName m)
        defs = [renderFrags (defBody d) | Line {lineBody = Define d} <- treeLines tree, defName d == name]
        perTarget = [(tsName t, fst (runEval (E.initialState (specEnv s)) (expandText (renderMacro m)))) | (t, s) <- specs]
        body =
          T.unlines $
            ["**" <> renderMacro m <> "**", ""]
              <> [targetLine n v | (n, v) <- perTarget]
              <> (if null defs then [] else ["", "defined in this spec as:"] <> ["```spec\n" <> d <> "\n```" | d <- defs])
     in Just (sp, body)
  Nothing -> case lineAtPos tree pos of
    Just l@Line {lineBody = Tag t} ->
      let values = [(tsName ts, teValue (locValue e)) | (ts, s) <- specs, p <- specPackages s, e <- pkgTags p, locSpan e == lineSpan l]
       in if null values then Nothing else Just (lineSpan l, T.unlines (("**" <> tagName t <> "**") : "" : [targetLine n v | (n, v) <- values]))
    Just l@Line {lineBody = Cond c} ->
      let results = [(tsName ts, condResult r) | (ts, s) <- specs, r <- specConditions s, condLineSpan r == lineSpan l]
       in if null results then Nothing else Just (lineSpan l, T.unlines (("**" <> condKeyword c <> "**") : "" : [n <> ": " <> triText r | (n, r) <- results]))
    _ -> Nothing
  where
    targetLine n v = "- " <> n <> ": " <> describeValue v
    triText = \case
      E.Yes -> "true"
      E.No -> "false"
      E.Unknown rs -> "unknown (" <> T.intercalate ", " (map describeReason rs) <> ")"

definitionSpans :: Tree -> Pos -> [Span]
definitionSpans tree pos = case macroAt tree pos of
  Nothing -> []
  Just (_, _, m) ->
    let name = fromMaybe "" (macroName m)
        defines = [lineSpan l | l@Line {lineBody = Define d} <- treeLines tree, defName d == name]
        tagDefs =
          [ lineSpan l
          | l@Line {lineBody = Tag t} <- treeLines tree,
            let tn = T.toLower (tagName t),
            tn == name && tagDefinesMacro tn || sourceTag tn name
          ]
        bconds = [lineSpan l | l@Line {lineBody = Define d} <- treeLines tree, defKind d `elem` [DefBcond, DefBcondWith, DefBcondWithout], "with_" <> defName d == name || withArg m == Just (defName d)]
     in defines <> tagDefs <> bconds
  where
    sourceTag tn name = case (T.stripPrefix "SOURCE" name, T.stripPrefix "PATCH" name) of
      (Just n, _) -> tn == "source" <> n || (n == "0" && tn == "source")
      (_, Just n) -> tn == "patch" <> n || (n == "0" && tn == "patch")
      _ -> False
    withArg = \case
      Braced _ n (Just (SpaceArg fs)) | n `elem` ["with", "without"] -> Just (T.strip (renderFrags fs))
      _ -> Nothing

symbols :: Doc -> Tree -> [J.DocumentSymbol]
symbols doc tree = go (treeLines tree)
  where
    allLines = treeLines tree
    go = \case
      [] -> []
      l@Line {lineBody = Header h} : rest ->
        let (inside, _) = break isHeader rest
            end = case reverse inside of
              x : _ -> spanEnd (lineSpan x)
              [] -> spanEnd (lineSpan l)
            name = T.strip (secKeyword h <> renderFrags (secArgs h))
            kind = case secKind h of
              SPackage -> J.SymbolKind_Package
              k | isScriptSection k -> J.SymbolKind_Event
              k | isBuildSection k -> J.SymbolKind_Function
              _ -> J.SymbolKind_Namespace
         in sym name kind (Span (spanStart (lineSpan l)) end) (lineSpan l) (children inside) : go rest
      l@Line {lineBody = Define d} : rest
        | not (insideSection l) -> sym ("%" <> defName d) J.SymbolKind_Variable (lineSpan l) (lineSpan l) [] : go rest
      _ : rest -> go rest
    insideSection l = any (\h -> posOffset (spanStart (lineSpan h)) < posOffset (spanStart (lineSpan l))) [h | h@Line {lineBody = Header _} <- allLines]
    children inside = [sym (tagName t) J.SymbolKind_Property (lineSpan l) (lineSpan l) [] | l@Line {lineBody = Tag t} <- inside]
    isHeader = \case
      Line {lineBody = Header _} -> True
      _ -> False
    sym name kind range sel kids =
      J.DocumentSymbol
        { J._name = if T.null name then "(unnamed)" else name,
          J._detail = Nothing,
          J._kind = kind,
          J._tags = Nothing,
          J._deprecated = Nothing,
          J._range = toRange doc range,
          J._selectionRange = toRange doc sel,
          J._children = if null kids then Nothing else Just kids
        }

tagCompletions :: [Text]
tagCompletions =
  [ "Name",
    "Version",
    "Release",
    "Epoch",
    "Summary",
    "License",
    "SourceLicense",
    "URL",
    "VCS",
    "BugURL",
    "Source",
    "Patch",
    "BuildRequires",
    "BuildConflicts",
    "BuildArch",
    "BuildSystem",
    "BuildOption",
    "ExclusiveArch",
    "ExcludeArch",
    "Requires",
    "Recommends",
    "Suggests",
    "Supplements",
    "Enhances",
    "Provides",
    "Conflicts",
    "Obsoletes",
    "OrderWithRequires",
    "Packager",
    "AutoReq",
    "AutoProv",
    "AutoReqProv"
  ]

completions :: Doc -> Tree -> [(TargetSpec, Spec)] -> J.Position -> [J.CompletionItem]
completions doc tree specs (J.Position l c) =
  let currentLine = maybe "" snd (lineAt (docIndex doc) (fromIntegral l))
      Pos _ _ col = fromUtf16 (docIndex doc) (fromIntegral l) (fromIntegral c)
      before = T.take col currentLine
      section = sectionAt (fromIntegral l)
      (wordPart, _) = T.breakOnEnd "%" before
      afterPercent = T.dropWhile (`elem` ['{', '?', '!']) (T.takeWhileEnd (\ch -> ch /= '%') before)
   in if
        | T.null wordPart && T.all (\ch -> ch /= ' ' && ch /= '\t') before && isPreambleLike section ->
            [item t J.CompletionItemKind_Property (Just (t <> ": ")) | t <- tagCompletions]
        | T.null wordPart && T.all (\ch -> ch /= ' ' && ch /= '\t') before -> []
        | "%" == before || (T.isPrefixOf "%" before && T.all (\ch -> ch /= ' ' && ch /= '{') before) ->
            [item ("%" <> n) J.CompletionItemKind_Keyword (Just n) | n <- keywords]
              <> macroItems afterPercent
        | not (T.any (== ' ') afterPercent) -> macroItems afterPercent
        | otherwise -> []
  where
    keywords = map fst sectionKinds <> ["if", "ifarch", "ifnarch", "ifos", "ifnos", "elif", "elifarch", "elifos", "else", "endif", "global", "define", "undefine", "bcond", "include"]
    sectionAt lineNo = foldl (\acc x -> case x of Line {lineBody = Header h} | posLine (spanStart (lineSpan x)) < lineNo -> Just (secKind h); _ -> acc) Nothing (treeLines tree)
    macroItems prefix =
      [ item n J.CompletionItemKind_Variable (Just n)
      | n <- names,
        prefix `T.isPrefixOf` n
      ]
    names =
      Map.keys (Map.fromList [(n, ()) | (_, s) <- take 1 specs, n <- Map.keys (E.envMacros (specEnv s))])
        <> builtinNames
        <> [n | (n, _, _) <- preludeMacros]
    item label kind insert =
      J.CompletionItem
        { J._label = label,
          J._labelDetails = Nothing,
          J._kind = Just kind,
          J._tags = Nothing,
          J._detail = Nothing,
          J._documentation = Nothing,
          J._deprecated = Nothing,
          J._preselect = Nothing,
          J._sortText = Nothing,
          J._filterText = Nothing,
          J._insertText = insert,
          J._insertTextFormat = Nothing,
          J._insertTextMode = Nothing,
          J._textEdit = Nothing,
          J._textEditText = Nothing,
          J._additionalTextEdits = Nothing,
          J._commitCharacters = Nothing,
          J._command = Nothing,
          J._data_ = Nothing
        }

semanticTokens :: Doc -> Tree -> [J.SemanticTokenAbsolute]
semanticTokens doc tree = sortOn (\(J.SemanticTokenAbsolute l c _ _ _) -> (l, c)) (concatMap lineTokens (treeLines tree))
  where
    lineTokens l = case lineBody l of
      Comment _ _ -> token (lineSpan l) J.SemanticTokenTypes_Comment
      Tag t ->
        let s = advancePos (spanStart (lineSpan l)) (tagIndent t)
         in token (Span s (advancePos s (tagName t))) J.SemanticTokenTypes_Property <> macros l
      Define d ->
        let s = advancePos (spanStart (lineSpan l)) (defIndent d)
            kwEnd = advancePos s (defKeyword d)
            nameStart = advancePos kwEnd (defGap d)
         in token (Span s kwEnd) J.SemanticTokenTypes_Keyword
              <> token (Span nameStart (advancePos nameStart (defName d))) J.SemanticTokenTypes_Variable
              <> macros l
      Cond c ->
        let s = advancePos (spanStart (lineSpan l)) (condIndent c)
         in token (Span s (advancePos s (condKeyword c))) J.SemanticTokenTypes_Keyword <> macros l
      Header h ->
        let s = spanStart (lineSpan l)
         in token (Span s (advancePos s (secKeyword h))) J.SemanticTokenTypes_Namespace <> macros l
      Include prefix _ ->
        let s = spanStart (lineSpan l)
         in token (Span s (advancePos s prefix)) J.SemanticTokenTypes_Keyword <> macros l
      _ -> macros l
    macros l = concat [token sp J.SemanticTokenTypes_Macro | (p, fs) <- lineFrags l, (sp, _) <- macroSpans p fs]
    token (Span s e) kind
      | posLine s /= posLine e = []
      | otherwise =
          let (line, start) = toUtf16 (docIndex doc) s
              (_, end) = toUtf16 (docIndex doc) e
           in [J.SemanticTokenAbsolute (fromIntegral line) (fromIntegral start) (fromIntegral (end - start)) kind [] | end > start]

isNameChar' :: Char -> Bool
isNameChar' c = c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

nameSpans :: Tree -> Name -> [Span]
nameSpans tree n =
  [ Span s (advancePos s n)
  | l@Line {lineBody = Define d} <- treeLines tree,
    defName d == n,
    let s = advancePos (spanStart (lineSpan l)) (defIndent d <> defKeyword d <> defGap d)
  ]
    <> [ Span s (advancePos s n)
       | l <- treeLines tree,
         (p, fs) <- lineFrags l,
         (Span start _, m) <- macroSpans p fs,
         Just (prefix, name') <- [refPrefix m],
         name' == n,
         let s = advancePos start prefix
       ]
  where
    refPrefix = \case
      Bare f nm -> Just ("%" <> f, nm)
      Braced f nm _ -> Just ("%{" <> f, nm)
      _ -> Nothing

symbolAt :: Tree -> Pos -> Maybe Name
symbolAt tree pos = case macroAt tree pos of
  Just (_, _, m) -> macroName m
  Nothing -> case lineAtPos tree pos of
    Just l@Line {lineBody = Define d} ->
      let s = advancePos (spanStart (lineSpan l)) (defIndent d <> defKeyword d <> defGap d)
       in if spanContains (Span s (advancePos s (defName d))) pos then Just (defName d) else Nothing
    _ -> Nothing

occurrences :: Tree -> Pos -> [Span]
occurrences tree pos = maybe [] (nameSpans tree) (symbolAt tree pos)

renameTarget :: Tree -> Pos -> Maybe (Span, Name)
renameTarget tree pos = do
  n <- symbolAt tree pos
  if null [() | Line {lineBody = Define d} <- treeLines tree, defName d == n, defKind d `elem` [DefGlobal, DefDefine]]
    then Nothing
    else listToMaybe [(sp, n) | sp <- nameSpans tree n, spanContains sp pos]

inlayHints :: Doc -> Tree -> [(TargetSpec, Spec)] -> [J.InlayHint]
inlayHints doc tree specs = case specs of
  [] -> []
  (_, spec) : _ ->
    [ hint (spanEnd (tagValueSpan' l t)) v
    | l@Line {lineBody = Tag t} <- treeLines tree,
      any isMacro (tagValue t),
      v <- take 1 [teValue (locValue e) | p <- specPackages spec, e <- pkgTags p, locSpan e == lineSpan l]
    ]
  where
    isMacro = \case
      Macro _ -> True
      _ -> False
    tagValueSpan' l t =
      let start = advancePos (spanStart (lineSpan l)) (tagIndent t <> tagName t <> maybe "" (\q -> qualGap q <> qualRaw q) (tagQualifier t) <> tagSep t)
       in Span start (advancePos start (renderFrags (tagValue t)))
    hint pos v =
      let (line, col) = toUtf16 (docIndex doc) pos
       in J.InlayHint
            { J._position = J.Position (fromIntegral line) (fromIntegral col),
              J._label = J.InL ("= " <> T.take 80 (showValue v)),
              J._kind = Nothing,
              J._textEdits = Nothing,
              J._tooltip = Nothing,
              J._paddingLeft = Just True,
              J._paddingRight = Nothing,
              J._data_ = Nothing
            }

evrLenses :: Doc -> Tree -> [(TargetSpec, Spec)] -> [J.CodeLens]
evrLenses doc tree specs =
  [ J.CodeLens (toRange doc (lineSpan l)) (Just (J.Command title "" Nothing)) Nothing
  | l <- take 1 [x | x@Line {lineBody = Tag t} <- treeLines tree, T.toLower (tagName t) == "name"],
    let title = T.intercalate "  \x2022  " [tsName ts <> ": " <> evrText s | (ts, s) <- specs]
  ]
  where
    evrText s = case Q.evr s of
      Q.Resolved e -> renderEVR e
      Q.Unresolved rs -> "? (" <> T.intercalate ", " (map describeReason (take 2 rs)) <> ")"
