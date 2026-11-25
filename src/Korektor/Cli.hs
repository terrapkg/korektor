module Korektor.Cli
  ( Command (..),
    LintOptions (..),
    parseCommand,
    runCommand,
    settingsFor,
    Settings (..),
    lintSource,
    applySafeFixes,
    EnvCache,
    newEnvCache,
    envFor,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (forM, unless, when)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
import Korektor.AndaCi (readAndaCi)
import Korektor.Config
import Korektor.Discover (findSpecs)
import Korektor.Output
import Korektor.Tools
import Options.Applicative
import Paths_korektor (version)
import Rozbir.Edit (TextEdit (..), apply)
import Rozbir.Eval (Env, newEnv)
import Rozbir.IO (loadSystemMacros, resolveIncludes)
import Rozbir.Lint
import Rozbir.Syntax (Pos (..), Span (..))
import Rozbir.Syntax.Parse (parse)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)

data LintOptions = LintOptions
  { loPaths :: [FilePath],
    loAndaCi :: Maybe FilePath,
    loConfig :: Maybe FilePath,
    loRuleSets :: [RuleSet],
    loTargets :: [Text],
    loFormat :: Format,
    loFailOn :: Maybe Severity,
    loFix :: Bool,
    loSystemMacros :: Maybe Bool,
    loEnable :: [RuleId],
    loDisable :: [RuleId]
  }

data Command
  = CmdLint LintOptions
  | CmdRules Bool
  | CmdLsp
  | CmdQuery QueryOptions
  | CmdEval EvalOptions
  | CmdEdit EditOptions

parseCommand :: IO Command
parseCommand =
  execParser $
    info
      (commands <**> helper <**> simpleVersioner ("korektor " <> showVersion version))
      (fullDesc <> progDesc "lint RPM spec files, or serve diagnostics over LSP")
  where
    commands =
      hsubparser
        ( command "lint" (info (CmdLint <$> lintOptions) (progDesc "lint spec files and directories"))
            <> command "rules" (info (CmdRules <$> switch (long "markdown" <> help "print a markdown table")) (progDesc "list every rule"))
            <> command "lsp" (info (pure CmdLsp) (progDesc "run the language server on stdio"))
            <> command "query" (info (CmdQuery <$> queryOptions) (progDesc "print what rozbir knows about a spec: name, EVR, sources, bconds, BuildRequires"))
            <> command "eval" (info (CmdEval <$> evalOptions) (progDesc "expand macros the way rpm would after reading a spec, without running anything"))
            <> command "edit" (info (CmdEdit <$> editOptions) (progDesc "set Version, tags or macros, bump Release and add changelog entries with minimal diffs"))
        )
    lintOptions =
      LintOptions
        <$> many (strArgument (metavar "PATH..." <> help "spec files or directories to search (default: ., or none with --anda-ci)"))
        <*> optional (strOption (long "anda-ci" <> metavar "FILE" <> help "lint the projects in this `anda ci` output (- for stdin)"))
        <*> optional (strOption (long "config" <> metavar "FILE" <> help "use this korektor.toml instead of searching for one"))
        <*> many (option (maybeReader (parseRuleSet . T.pack)) (long "ruleset" <> metavar "SET" <> help "enable a ruleset (core, rpmlint, terra), repeatable"))
        <*> many (T.pack <$> strOption (long "target" <> metavar "TARGET" <> help "evaluate for a target, like f44, el10:aarch64 or unknown, repeatable"))
        <*> option (eitherReader parseFormat) (long "format" <> metavar "FORMAT" <> value FormatText <> help "text, json, sarif or github")
        <*> optional (option (maybeReader (parseSeverity . T.pack)) (long "fail-on" <> metavar "SEVERITY" <> help "exit 1 on findings at or above this (info, warning, error)"))
        <*> switch (long "fix" <> help "apply safe fixes in place")
        <*> optional (flag' True (long "system-macros" <> help "load rpm's macro files, so undefined macros are really undefined") <|> flag' False (long "no-system-macros"))
        <*> many (T.pack <$> strOption (long "enable" <> metavar "RULE" <> help "turn on a rule outside the enabled rulesets"))
        <*> many (T.pack <$> strOption (long "disable" <> metavar "RULE" <> help "turn off a rule"))

data Settings = Settings
  { stLint :: !LintConfig,
    stTargets :: ![TargetSpec],
    stSystemMacros :: !Bool,
    stFailOn :: !Severity
  }

cliConfig :: LintOptions -> Config
cliConfig LintOptions {..} =
  emptyConfig
    { cfgRuleSets = if null loRuleSets then Nothing else Just loRuleSets,
      cfgRules = Map.fromList ([(r, Nothing) | r <- loDisable] <> [(r, maybe (Just Warn) (Just . ruleSeverity) (findRule r)) | r <- loEnable]),
      cfgFailOn = loFailOn,
      cfgSystemMacros = loSystemMacros,
      cfgTargets = if null loTargets then Nothing else Just loTargets
    }

settingsFor :: Maybe FilePath -> Config -> FilePath -> IO (Either Text Settings)
settingsFor explicit overrides spec = do
  fileCfg <- case explicit of
    Just path -> loadConfigFile path
    Nothing ->
      findConfigFile (takeDirectory spec) >>= \case
        Just path -> loadConfigFile path
        Nothing -> pure (Right emptyConfig)
  andaCfg <- do
    let hcl = takeDirectory spec </> "anda.hcl"
    exists <- doesFileExist hcl
    if not exists
      then pure emptyConfig
      else do
        r <- try (BS.readFile hcl) :: IO (Either IOException BS.ByteString)
        pure $ case r of
          Right bytes -> emptyConfig {cfgRules = Map.fromList [(rid, Nothing) | rid <- andaIgnores (decodeUtf8Lenient bytes)]}
          Left _ -> emptyConfig
  pure $ do
    base <- fileCfg
    let cfg = base <> andaCfg <> overrides
    targets <- targetsOf cfg
    Right (Settings (lintConfig cfg) targets (fromMaybe False (cfgSystemMacros cfg)) (failOnOf cfg))

type EnvCache = IORef (Map (Maybe Text) Env)

newEnvCache :: IO EnvCache
newEnvCache = newIORef Map.empty

envFor :: EnvCache -> Bool -> TargetSpec -> IO Env
envFor cache systemMacros t
  | not systemMacros = pure (newEnv False)
  | otherwise = do
      cached <- Map.lookup (tsArch t) <$> readIORef cache
      case cached of
        Just env -> pure env
        Nothing -> do
          env <- loadSystemMacros (tsArch t)
          modifyIORef' cache (Map.insert (tsArch t) env)
          pure env

lintSource :: EnvCache -> Settings -> FilePath -> Text -> IO [Diagnostic]
lintSource cache Settings {..} path src = do
  let tree = parse src
  includes <- resolveIncludes path tree
  envs <- mapM (\t -> (,toTarget t) <$> envFor cache stSystemMacros t) stTargets
  let perTarget = [lintTree stLint env [target] includes tree | (env, target) <- envs]
  pure (dedupe (concat perTarget))
  where
    dedupe = go []
      where
        go seen = \case
          [] -> reverse seen
          d : ds
            | any (same d) seen -> go seen ds
            | otherwise -> go (d : seen) ds
        same a b = diagRule a == diagRule b && diagSpan a == diagSpan b && diagMessage a == diagMessage b

applySafeFixes :: [Diagnostic] -> Text -> Maybe Text
applySafeFixes diags src =
  let edits = pick [] (sortOn (posOffset . spanStart . editSpan) [e | d <- diags, f <- take 1 (diagFixes d), fixSafe f, e <- fixEdits f])
   in if null edits then Nothing else Just (apply edits src)
  where
    pick acc = \case
      [] -> reverse acc
      e : es
        | any (overlaps e) acc -> pick acc es
        | otherwise -> pick (e : acc) es
    overlaps a b =
      let s1 = posOffset (spanStart (editSpan a))
          e1 = posOffset (spanEnd (editSpan a))
          s2 = posOffset (spanStart (editSpan b))
          e2 = posOffset (spanEnd (editSpan b))
       in s1 < e2 && s2 < e1 || s1 == s2

runCommand :: Command -> IO ExitCode
runCommand = \case
  CmdRules False -> do
    mapM_ (\r -> TIO.putStrLn (T.intercalate "\t" [ruleId r, ruleSetName (ruleSet r), sev (ruleSeverity r), ruleDoc r])) allRules
    pure ExitSuccess
  CmdRules True -> do
    TIO.putStr (rulesMarkdown sev)
    pure ExitSuccess
  CmdLsp -> pure ExitSuccess
  CmdQuery o -> runQuery o
  CmdEval o -> runEval' o
  CmdEdit o -> runEdit o
  CmdLint opts -> lint opts
  where
    sev = \case
      Error -> "error"
      Warn -> "warning"
      Info -> "info"

lint :: LintOptions -> IO ExitCode
lint opts =
  lintPaths opts >>= \case
    Left err -> do
      TIO.hPutStrLn stderr ("korektor: " <> err)
      pure (ExitFailure 2)
    Right paths -> lintFound opts =<< findSpecs paths

lintPaths :: LintOptions -> IO (Either Text [FilePath])
lintPaths opts = case loAndaCi opts of
  Nothing -> pure (Right (if null (loPaths opts) then ["."] else loPaths opts))
  Just source -> fmap (loPaths opts <>) <$> readAndaCi source

lintFound :: LintOptions -> Either FilePath [FilePath] -> IO ExitCode
lintFound opts found =
  case found of
    Left missing -> do
      TIO.hPutStrLn stderr ("korektor: " <> T.pack missing <> " doesn't exist")
      pure (ExitFailure 2)
    Right specs -> do
      cache <- newIORef Map.empty
      failed <- newIORef False
      results <- forM specs $ \path -> do
        settings <- settingsFor (loConfig opts) (cliConfig opts) path
        case settings of
          Left err -> do
            TIO.hPutStrLn stderr ("korektor: " <> err)
            modifyIORef' failed (const True)
            pure Nothing
          Right st -> do
            src <- decodeUtf8Lenient <$> BS.readFile path
            diags0 <- lintSource cache st path src
            (finalSrc, diags) <-
              if loFix opts
                then fixLoop cache st path src diags0 (3 :: Int)
                else pure (src, diags0)
            when (loFix opts && finalSrc /= src) (BS.writeFile path (encodeUtf8 finalSrc))
            pure (Just (FileResult path finalSrc diags, stFailOn st))
      hadErrors <- readIORef failed
      let ok = [r | Just r <- results]
          rendered = renderResults (loFormat opts) (map fst ok)
      unless (T.null rendered && loFormat opts /= FormatJson && loFormat opts /= FormatSarif) (TIO.putStr rendered)
      let failing = or [any ((>= threshold) . diagSeverity) (frDiagnostics fr) | (fr, threshold) <- ok]
      pure $
        if
          | hadErrors -> ExitFailure 2
          | failing -> ExitFailure 1
          | otherwise -> ExitSuccess
  where
    fixLoop cache st path src diags n = case applySafeFixes diags src of
      Just src' | n > 0 -> do
        diags' <- lintSource cache st path src'
        fixLoop cache st path src' diags' (n - 1)
      _ -> pure (src, diags)

rulesMarkdown :: (Severity -> Text) -> Text
rulesMarkdown sev =
  T.unlines $
    ["# korektor rules", "", "generated by `korektor rules --markdown`.", ""]
      <> concat
        [ ["## " <> ruleSetName set, "", "| rule | default | what it catches |", "| --- | --- | --- |"]
            <> ["| `" <> ruleId r <> "` | " <> sev (ruleSeverity r) <> " | " <> ruleDoc r <> " |" | r <- allRules, ruleSet r == set]
            <> [""]
        | set <- [minBound .. maxBound]
        ]
