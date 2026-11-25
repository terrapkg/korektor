module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Korektor.AndaCi (andaCiDirs, parseAndaCi)
import Korektor.Cli (Settings (..), applySafeFixes, lintSource, newEnvCache, settingsFor)
import Korektor.Config
import Korektor.Output
import Korektor.Position
import Korektor.Tools
import Rozbir.Eval
  ( Target (..),
    newEnv,
    unknownTarget,
  )
import Rozbir.Lint (Diagnostic (..), LintConfig (..), RuleSet (..), Severity (..))
import Rozbir.Spec (resolve)
import Rozbir.Syntax (Pos (..))
import Rozbir.Syntax qualified
import Rozbir.Syntax.Parse (parse)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

configText :: Text
configText =
  T.unlines
    [ "rulesets = [\"core\", \"terra\"]",
      "fail-on = \"error\"",
      "targets = [\"f44\", \"mine\"]",
      "",
      "[rules]",
      "terra-packager = \"off\"",
      "hardcoded-path = \"error\"",
      "",
      "[target.mine]",
      "arch = \"aarch64\"",
      "macros = { dist = \".um44\", rhel = false, fedora = 44 }"
    ]

spec :: Text
spec = T.unlines ["Name: x", "Version: 1", "Release: 1", "Summary: s", "License: MIT", "", "%description", "d", "", "%files", "/usr/bin/x"]

main :: IO ()
main = hspec $ do
  describe "config" $ do
    it "parses korektor.toml" $ case parseConfig configText of
      Right cfg -> do
        cfgRuleSets cfg `shouldBe` Just [Core, Terra]
        cfgFailOn cfg `shouldBe` Just Error
        Map.toList (cfgRules cfg) `shouldBe` [("hardcoded-path", Just Error), ("terra-packager", Nothing)]
        cfgTargetDefs cfg `shouldBe` [TargetSpec "mine" (Just "aarch64") [("dist", Just ".um44"), ("fedora", Just "44"), ("rhel", Nothing)]]
        map tsName <$> targetsOf cfg `shouldBe` Right ["f44", "mine"]
      Left e -> expectationFailure (T.unpack e)
    it "rejects a bad severity" $
      parseConfig "[rules]\nfoo = \"loud\"\n" `shouldBe` Left "unknown severity loud"
    it "knows the presets" $ do
      presetTarget "f44" `shouldBe` Just (TargetSpec "f44" (Just "x86_64") [("dist", Just ".fc44"), ("fedora", Just "44"), ("rhel", Nothing)])
      fmap tsArch (presetTarget "el10:aarch64") `shouldBe` Just (Just "aarch64")
      presetTarget "nope" `shouldBe` Nothing
    it "lets later layers win" $ do
      let a = emptyConfig {cfgFailOn = Just Info, cfgRules = Map.fromList [("x", Nothing)]}
          b = emptyConfig {cfgFailOn = Just Error, cfgRules = Map.fromList [("x", Just Warn)]}
      cfgFailOn (a <> b) `shouldBe` Just Error
      Map.lookup "x" (cfgRules (a <> b)) `shouldBe` Just (Just Warn)
    it "reads korektor_ignore from anda.hcl" $
      andaIgnores "project pkg {\n  labels {\n    korektor_ignore = \"hardcoded-path, terra-packager\"\n  }\n}\n" `shouldBe` ["hardcoded-path", "terra-packager"]
    it "turns a target into a rozbir target" $
      toTarget (TargetSpec "t" (Just "x86_64") [("a", Just "1")]) `shouldBe` Target (Just "x86_64") (Just "linux") [("a", Just "1")]

  describe "settings" $
    it "layers korektor.toml, anda.hcl and overrides" $
      withSystemTempDirectory "korektor" $ \dir -> do
        createDirectoryIfMissing True (dir </> ".git")
        createDirectoryIfMissing True (dir </> "pkg")
        writeFile (dir </> "korektor.toml") (T.unpack configText)
        writeFile (dir </> "pkg" </> "anda.hcl") "labels {\n  korektor_ignore = \"terra-dist-release\"\n}\n"
        writeFile (dir </> "pkg" </> "x.spec") (T.unpack spec)
        r <- settingsFor Nothing emptyConfig {cfgFailOn = Just Warn} (dir </> "pkg" </> "x.spec")
        case r of
          Left e -> expectationFailure (T.unpack e)
          Right st -> do
            stFailOn st `shouldBe` Warn
            lcRuleSets (stLint st) `shouldBe` Set.fromList [Core, Terra]
            Map.lookup "terra-dist-release" (lcOverrides (stLint st)) `shouldBe` Just Nothing
            cache <- newEnvCache
            diags <- lintSource cache st (dir </> "pkg" </> "x.spec") spec
            map (\x -> (diagRule x, diagSeverity x)) diags `shouldBe` [("hardcoded-path", Error)]

  describe "fixes" $
    it "applies safe fixes that don't overlap" $ do
      let st = Settings (LintConfig (Set.fromList [Terra]) Map.empty) [TargetSpec "unknown" Nothing []] False Warn
      cache <- newEnvCache
      diags <- lintSource cache st "x.spec" "Release: 1\n"
      applySafeFixes diags "Release: 1\n" `shouldBe` Just "Release: 1%{?dist}\n"

  describe "tools" $ do
    it "describes a spec" $ do
      let tree = parse "%global ver 1.2\nName: x\nVersion: %{ver}\nRelease: 1\nSource0: a-%{version}.tar.gz\n%bcond docs 1\n"
          (_, text) = describeQuery tree (resolve (newEnv False) unknownTarget tree)
      T.lines text `shouldBe` ["name: x", "evr: 1.2-1", "version inputs: %ver", "source0: a-1.2.tar.gz", "bcond docs: on"]
    it "edits a spec on disk" $
      withSystemTempDirectory "korektor" $ \dir -> do
        let path = dir </> "x.spec"
        writeFile path "%global ver 1.2\nName: x\nVersion: %{ver}\nRelease: 3%{?dist}\nPackager: A <a@b>\n\n%changelog\n"
        code <- runEdit (EditOptions path [SetVersion "1.3", Changelog "update"] Nothing False False)
        code `shouldBe` ExitSuccess
        out <- T.pack <$> readFile path
        all (`elem` T.lines out) ["%global ver 1.3", "Release: 1%{?dist}", "- update"] `shouldBe` True
        any (" A <a@b> - 1.3-1" `T.isSuffixOf`) (T.lines out) `shouldBe` True

  describe "anda ci" $ do
    let matrix = "build_matrix=[{\"pkg\":\"anda/apps/foo/pkg\",\"arch\":\"x86_64\",\"labels\":{}},{\"pkg\":\"anda/apps/foo/pkg\",\"arch\":\"aarch64\",\"labels\":{}},{\"pkg\":\"anda/lib/bar/pkg\",\"arch\":\"x86_64\",\"labels\":{\"subrepo\":\"extras\"}}]\n"
    it "reads the GITHUB_OUTPUT line, one entry per project" $
      parseAndaCi matrix `shouldBe` Right ["anda/apps/foo/pkg", "anda/lib/bar/pkg"]
    it "reads the bare JSON from a job output" $
      parseAndaCi (T.drop (T.length "build_matrix=") matrix) `shouldBe` Right ["anda/apps/foo/pkg", "anda/lib/bar/pkg"]
    it "treats an empty matrix as nothing to lint" $ do
      parseAndaCi "build_matrix=[]\n" `shouldBe` Right []
      parseAndaCi "" `shouldBe` Right []
    it "rejects anything else" $
      parseAndaCi "nope" `shouldSatisfy` either (const True) (const False)
    it "maps projects to their directories" $
      withSystemTempDirectory "korektor" $ \dir -> do
        createDirectoryIfMissing True (dir </> "anda" </> "foo")
        andaCiDirs [T.pack (dir </> "anda/foo/pkg"), T.pack (dir </> "anda/foo")] `shouldReturn` [dir </> "anda/foo"]

  describe "positions" $ do
    it "counts UTF-16 units" $ do
      let idx = mkIndex "a😀b\nc"
      toUtf16 idx (Pos 2 0 2) `shouldBe` (0, 3)
      fromUtf16 idx 0 3 `shouldBe` Pos 2 0 2
      fromUtf16 idx 1 1 `shouldBe` Pos 5 1 1

  describe "output" $ do
    let d = Diagnostic "hardcoded-path" Warn (spanAt 10 1 0 9) "use %{_bindir}" []
        spanAt o l c len = Rozbir.Syntax.Span (Pos o l c) (Pos (o + len) l (c + len))
        fr = FileResult "x.spec" "Name: x\n/usr/bin/x\n" [d]
    it "prints text" $
      renderResults FormatText [fr] `shouldBe` "x.spec:2:1: warning: use %{_bindir} [hardcoded-path]\n"
    it "prints GitHub annotations" $
      renderResults FormatGithub [fr] `shouldBe` "::warning file=x.spec,line=2,col=1,endLine=2,endColumn=10,title=korektor hardcoded-path::use %25{_bindir}\n"
    it "prints SARIF" $
      renderResults FormatSarif [fr] `shouldSatisfy` T.isInfixOf "\"ruleId\":\"hardcoded-path\""
