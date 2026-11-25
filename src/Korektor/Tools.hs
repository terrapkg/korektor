module Korektor.Tools
  ( QueryOptions (..),
    EvalOptions (..),
    EditOptions (..),
    EditAction (..),
    queryOptions,
    evalOptions,
    editOptions,
    runQuery,
    runEval',
    runEdit,
    describeQuery,
  )
where

import Control.Monad (foldM, unless)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8Lenient, encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime, utctDay)
import Korektor.Config (TargetSpec (..), presetTarget, toTarget)
import Options.Applicative
import Rozbir.Edit
import Rozbir.Eval (Env, expandText, initialState, newEnv, runEval, showValue)
import Rozbir.Eval qualified as E
import Rozbir.IO (loadSystemMacros, resolveIncludes)
import Rozbir.Query
import Rozbir.Spec
import Rozbir.Syntax (Tree)
import Rozbir.Syntax.Parse (parse)
import Rozbir.Version (renderEVR)
import System.Exit (ExitCode (..))
import System.IO (stderr)

data QueryOptions = QueryOptions
  { qoSpec :: FilePath,
    qoTarget :: Maybe Text,
    qoSystemMacros :: Bool,
    qoJson :: Bool
  }

data EvalOptions = EvalOptions
  { eoSpec :: FilePath,
    eoExpr :: Text,
    eoTarget :: Maybe Text,
    eoSystemMacros :: Bool
  }

data EditAction
  = SetVersion Text
  | SetTag Text Text
  | SetMacro Text Text
  | BumpRelease
  | Changelog Text

data EditOptions = EditOptions
  { edSpec :: FilePath,
    edActions :: [EditAction],
    edAuthor :: Maybe Text,
    edDryRun :: Bool,
    edLiteral :: Bool
  }

targetOption :: Parser (Maybe Text)
targetOption = optional (T.pack <$> strOption (long "target" <> metavar "TARGET" <> help "f44, el10:aarch64, rawhide or unknown (default: unknown)"))

systemMacrosOption :: Parser Bool
systemMacrosOption = switch (long "system-macros" <> help "load rpm's macro files from this machine")

queryOptions :: Parser QueryOptions
queryOptions =
  QueryOptions
    <$> strArgument (metavar "SPEC")
    <*> targetOption
    <*> systemMacrosOption
    <*> switch (long "json" <> help "print JSON")

evalOptions :: Parser EvalOptions
evalOptions =
  EvalOptions
    <$> strArgument (metavar "SPEC")
    <*> (T.pack <$> strArgument (metavar "EXPR" <> help "text to expand after the spec is read, like %{version} or %{_libdir}"))
    <*> targetOption
    <*> systemMacrosOption

editOptions :: Parser EditOptions
editOptions =
  EditOptions
    <$> strArgument (metavar "SPEC")
    <*> some editAction
    <*> optional (T.pack <$> strOption (long "author" <> metavar "NAME <EMAIL>" <> help "changelog author (default: the spec's Packager)"))
    <*> switch (long "dry-run" <> help "print the edited spec instead of writing it")
    <*> switch (long "literal" <> help "set tags where they are, without following them into %global lines")
  where
    editAction =
      (SetVersion . T.pack <$> strOption (long "set-version" <> metavar "VERSION" <> help "set Version (through its %global) and reset Release to 1"))
        <|> (pair SetTag <$> strOption (long "set-tag" <> metavar "TAG=VALUE" <> help "set a preamble tag"))
        <|> (pair SetMacro <$> strOption (long "set-macro" <> metavar "NAME=VALUE" <> help "set a %global or %define"))
        <|> flag' BumpRelease (long "bump-release" <> help "add one to Release")
        <|> (Changelog . T.pack <$> strOption (long "changelog" <> metavar "TEXT" <> help "add a changelog entry for the new EVR"))
    pair f raw = let (k, v) = T.breakOn "=" (T.pack raw) in f (T.strip k) (T.drop 1 v)

prepare :: Maybe Text -> Bool -> FilePath -> IO (Either Text (Tree, Spec, Env, E.Target))
prepare targetName systemMacros path = do
  src <- decodeUtf8Lenient <$> BS.readFile path
  case maybe (Just (TargetSpec "unknown" Nothing [])) presetTarget targetName of
    Nothing -> pure (Left ("unknown target " <> maybe "" id targetName))
    Just ts -> do
      env <- if systemMacros then loadSystemMacros (tsArch ts) else pure (newEnv False)
      let tree = parse src
          target = toTarget ts
      includes <- resolveIncludes path tree
      pure (Right (tree, resolveWith (ResolveOptions env target includes) tree, env, target))

resolvedJson :: (a -> Value) -> Resolved a -> Value
resolvedJson f = \case
  Resolved a -> object ["known" .= True, "value" .= f a]
  Unresolved rs -> object ["known" .= False, "reasons" .= map E.describeReason rs]

resolvedText :: (a -> Text) -> Resolved a -> Text
resolvedText f = \case
  Resolved a -> f a
  Unresolved rs -> "unknown (" <> T.intercalate ", " (map E.describeReason rs) <> ")"

describeQuery :: Tree -> Spec -> (Value, Text)
describeQuery tree spec =
  let srcs = [(srcNumber s, srcValue s) | s <- map locValue (sources spec)]
      pats = [(srcNumber s, srcValue s) | s <- map locValue (patches spec)]
      subs = map (showValue . pkgName) (subpackages spec)
      bcs = [(bcName b, bcEnabled b) | b <- map locValue (bconds spec)]
      brs = [(locValue d, not (null (locGuard d))) | d <- buildRequires spec]
      tri = \case
        E.Yes -> "on" :: Text
        E.No -> "off"
        E.Unknown _ -> "unknown"
      json =
        object
          [ "name" .= resolvedJson toJSON (name spec),
            "evr" .= resolvedJson (toJSON . renderEVR) (evr spec),
            "versionInputs" .= versionInputs tree,
            "sources" .= [object ["number" .= n, "value" .= v] | (n, v) <- srcs],
            "patches" .= [object ["number" .= n, "value" .= v] | (n, v) <- pats],
            "subpackages" .= subs,
            "bconds" .= [object ["name" .= n, "enabled" .= tri t] | (n, t) <- bcs],
            "buildRequires" .= [object ["dep" .= d, "conditional" .= c] | (d, c) <- brs]
          ]
      text =
        T.unlines $
          [ "name: " <> resolvedText id (name spec),
            "evr: " <> resolvedText renderEVR (evr spec),
            "version inputs: " <> T.unwords (map ("%" <>) (versionInputs tree))
          ]
            <> ["source" <> T.pack (show n) <> ": " <> showValue v | (n, v) <- srcs]
            <> ["patch" <> T.pack (show n) <> ": " <> showValue v | (n, v) <- pats]
            <> ["subpackage: " <> s | s <- subs]
            <> ["bcond " <> n <> ": " <> tri t | (n, t) <- bcs]
            <> ["buildrequires: " <> d <> (if c then " (conditional)" else "") | (d, c) <- brs]
   in (json, text)

runQuery :: QueryOptions -> IO ExitCode
runQuery QueryOptions {..} =
  prepare qoTarget qoSystemMacros qoSpec >>= \case
    Left e -> TIO.hPutStrLn stderr ("korektor: " <> e) >> pure (ExitFailure 2)
    Right (tree, spec, _, _) -> do
      let (json, text) = describeQuery tree spec
      if qoJson then TIO.putStrLn (decodeUtf8 (LBS.toStrict (encode json))) else TIO.putStr text
      pure ExitSuccess

runEval' :: EvalOptions -> IO ExitCode
runEval' EvalOptions {..} =
  prepare eoTarget eoSystemMacros eoSpec >>= \case
    Left e -> TIO.hPutStrLn stderr ("korektor: " <> e) >> pure (ExitFailure 2)
    Right (_, spec, _, _) -> do
      let v = fst (runEval (initialState (specEnv spec)) (expandText eoExpr))
      case E.knownText v of
        Just t -> TIO.putStrLn t >> pure ExitSuccess
        Nothing -> do
          TIO.putStrLn (showValue v)
          TIO.hPutStrLn stderr ("korektor: part of this can't be known without running it: " <> T.intercalate ", " (map E.describeReason (E.reasons v)))
          pure (ExitFailure 1)

runEdit :: EditOptions -> IO ExitCode
runEdit EditOptions {..} = do
  src0 <- decodeUtf8Lenient <$> BS.readFile edSpec
  today <- utctDay <$> getCurrentTime
  result <- foldM (step today) (Right src0) edActions
  case result of
    Left e -> TIO.hPutStrLn stderr ("korektor: " <> e) >> pure (ExitFailure 1)
    Right src -> do
      if edDryRun then TIO.putStr src else unless (src == src0) (BS.writeFile edSpec (encodeUtf8 src))
      pure ExitSuccess
  where
    step _ (Left e) _ = pure (Left e)
    step today (Right src) act = do
      let tree = parse src
          applied = fmap (`apply` src) . either (Left . describeEditError) Right
      pure $ case act of
        SetVersion v -> applied (setVersion v tree)
        SetTag k v -> applied ((if edLiteral then setTagLiteral else setTag) k v tree)
        SetMacro k v -> applied (setMacro k v tree)
        BumpRelease -> applied (bumpRelease tree)
        Changelog msg ->
          let spec = resolve (newEnv False) (E.Target Nothing (Just "linux") [("dist", Nothing)]) tree
              author = case edAuthor of
                Just a -> Right a
                Nothing -> case mainPackage spec >>= packageTag "packager" of
                  Just e | Just t <- E.knownText (teValue (locValue e)) -> Right (T.strip t)
                  _ -> Left "no --author and no Packager tag to take the changelog author from"
              evrText = case evr spec of
                Resolved e -> Just (renderEVR e)
                Unresolved _ -> Nothing
           in author >>= \a -> applied (addChangelog (ChangelogInput today a evrText [msg]) tree)
