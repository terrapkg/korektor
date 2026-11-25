module Korektor.Config
  ( Config (..),
    TargetSpec (..),
    emptyConfig,
    parseConfig,
    loadConfigFile,
    findConfigFile,
    andaIgnores,
    lintConfig,
    targetsOf,
    presetTarget,
    toTarget,
    failOnOf,
  )
where

import Control.Exception (IOException, try)
import Control.Monad ((>=>))
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Rozbir.Eval (Target (..), unknownTarget)
import Rozbir.Lint (LintConfig (..), RuleId, RuleSet (..), Severity (..), parseRuleSet, parseSeverity)
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import Toml (Table' (..), Value' (..))
import Toml qualified

data TargetSpec = TargetSpec
  { tsName :: !Text,
    tsArch :: !(Maybe Text),
    tsMacros :: ![(Text, Maybe Text)]
  }
  deriving stock (Eq, Show)

data Config = Config
  { cfgRuleSets :: !(Maybe [RuleSet]),
    cfgRules :: !(Map RuleId (Maybe Severity)),
    cfgFailOn :: !(Maybe Severity),
    cfgSystemMacros :: !(Maybe Bool),
    cfgTargets :: !(Maybe [Text]),
    cfgTargetDefs :: ![TargetSpec]
  }
  deriving stock (Eq, Show)

instance Semigroup Config where
  a <> b =
    Config
      { cfgRuleSets = maybe (cfgRuleSets a) Just (cfgRuleSets b),
        cfgRules = Map.union (cfgRules b) (cfgRules a),
        cfgFailOn = maybe (cfgFailOn a) Just (cfgFailOn b),
        cfgSystemMacros = maybe (cfgSystemMacros a) Just (cfgSystemMacros b),
        cfgTargets = maybe (cfgTargets a) Just (cfgTargets b),
        cfgTargetDefs = cfgTargetDefs b <> cfgTargetDefs a
      }

instance Monoid Config where
  mempty = emptyConfig

emptyConfig :: Config
emptyConfig = Config Nothing Map.empty Nothing Nothing Nothing []

parseConfig :: Text -> Either Text Config
parseConfig src = do
  MkTable top <- either (Left . T.pack) Right (Toml.parse src)
  let field k = snd <$> Map.lookup k top
  ruleSets <- traverse (textList "rulesets" >=> traverse ruleSet) (field "rulesets")
  rules <- maybe (Right Map.empty) (table "rules" >=> fmap Map.fromList . traverse rule . Map.toList) (field "rules")
  failOn <- traverse (text "fail-on" >=> severity) (field "fail-on")
  systemMacros <- traverse (bool "system-macros") (field "system-macros")
  targets <- traverse (textList "targets") (field "targets")
  targetDefs <- maybe (Right []) (table "target" >=> traverse targetDef . Map.toList) (field "target")
  pure (Config ruleSets rules failOn systemMacros targets targetDefs)
  where
    text k = \case
      Text' _ t -> Right t
      _ -> Left (k <> " should be a string")
    bool k = \case
      Bool' _ b -> Right b
      _ -> Left (k <> " should be true or false")
    textList k = \case
      List' _ vs -> traverse (text k) vs
      _ -> Left (k <> " should be a list of strings")
    table k = \case
      Table' _ (MkTable m) -> Right (Map.map snd m)
      _ -> Left (k <> " should be a table")
    ruleSet t = maybe (Left ("unknown ruleset " <> t)) Right (parseRuleSet t)
    severity t = maybe (Left ("unknown severity " <> t)) Right (parseSeverity t)
    rule (rid, v) = do
      t <- text ("rules." <> rid) v
      if T.toLower t `elem` ["off", "none", "false"]
        then Right (rid, Nothing)
        else (\s -> (rid, Just s)) <$> severity t
    targetDef (name, v) = do
      m <- table ("target." <> name) v
      arch <- traverse (text ("target." <> name <> ".arch")) (Map.lookup "arch" m)
      macros <- maybe (Right Map.empty) (table ("target." <> name <> ".macros")) (Map.lookup "macros" m)
      pairs <- traverse macroPair (Map.toList macros)
      Right (TargetSpec name arch pairs)
    macroPair (k, v) = case v of
      Text' _ t -> Right (k, Just t)
      Integer' _ i -> Right (k, Just (T.pack (show i)))
      Bool' _ False -> Right (k, Nothing)
      _ -> Left ("macro " <> k <> " should be a string, a number, or false to undefine it")

loadConfigFile :: FilePath -> IO (Either Text Config)
loadConfigFile path = do
  r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure $ case r of
    Left e -> Left (T.pack (show e))
    Right bytes -> either (\e -> Left (T.pack path <> ": " <> e)) Right (parseConfig (decodeUtf8Lenient bytes))

findConfigFile :: FilePath -> IO (Maybe FilePath)
findConfigFile start = makeAbsolute start >>= go
  where
    go dir = do
      let candidate = dir </> "korektor.toml"
      found <- doesFileExist candidate
      isRepoRoot <- doesDirectoryExist (dir </> ".git")
      let parent = takeDirectory dir
      if
        | found -> pure (Just candidate)
        | isRepoRoot || parent == dir -> pure Nothing
        | otherwise -> go parent

andaIgnores :: Text -> [RuleId]
andaIgnores hcl =
  concat
    [ filter (not . T.null) (map T.strip (T.splitOn "," v))
    | l <- map T.strip (T.lines hcl),
      Just rest <- [T.stripPrefix "korektor_ignore" l],
      Just afterEq <- [T.stripPrefix "=" (T.stripStart rest)],
      let raw = T.stripStart afterEq,
      Just quoted <- [T.stripPrefix "\"" raw],
      let v = T.takeWhile (/= '"') quoted
    ]

lintConfig :: Config -> LintConfig
lintConfig cfg =
  LintConfig
    { lcRuleSets = Set.fromList (fromMaybe [Core, Rpmlint] (cfgRuleSets cfg)),
      lcOverrides = cfgRules cfg
    }

failOnOf :: Config -> Severity
failOnOf = fromMaybe Warn . cfgFailOn

presetTarget :: Text -> Maybe TargetSpec
presetTarget raw =
  let (name, archPart) = T.breakOn ":" raw
      arch = if T.null archPart then Nothing else Just (T.drop 1 archPart)
      digits p = T.stripPrefix p name >>= \n -> if not (T.null n) && T.all isDigit n then Just n else Nothing
   in if
        | name == "unknown" -> Just (TargetSpec raw arch [])
        | name == "rawhide" -> Just (TargetSpec raw (Just (fromMaybe "x86_64" arch)) [("dist", Just ".fc45"), ("fedora", Just "45")])
        | Just n <- digits "f" -> Just (TargetSpec raw (Just (fromMaybe "x86_64" arch)) [("dist", Just (".fc" <> n)), ("fedora", Just n), ("rhel", Nothing)])
        | Just n <- digits "el" -> Just (TargetSpec raw (Just (fromMaybe "x86_64" arch)) [("dist", Just (".el" <> n)), ("rhel", Just n), ("el" <> n, Just "1"), ("fedora", Nothing)])
        | otherwise -> Nothing

toTarget :: TargetSpec -> Target
toTarget TargetSpec {..} = Target tsArch (targetOs unknownTarget) tsMacros

targetsOf :: Config -> Either Text [TargetSpec]
targetsOf cfg = case cfgTargets cfg of
  Nothing -> Right [TargetSpec "unknown" Nothing []]
  Just [] -> Right [TargetSpec "unknown" Nothing []]
  Just names -> traverse resolveName names
  where
    resolveName n = case listToMaybe [t | t <- cfgTargetDefs cfg, tsName t == n] of
      Just t -> Right t
      Nothing -> maybe (Left ("unknown target " <> n <> ", define it under [target." <> n <> "]")) Right (presetTarget n)
