module Korektor.Output
  ( Format (..),
    parseFormat,
    FileResult (..),
    renderResults,
  )
where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Korektor.Position (mkIndex, toUtf16)
import Rozbir.Lint (Diagnostic (..), Rule (..), Severity (..), findRule)
import Rozbir.Syntax (Span (..))

data Format = FormatText | FormatJson | FormatSarif | FormatGithub
  deriving stock (Eq, Show)

parseFormat :: String -> Either String Format
parseFormat = \case
  "text" -> Right FormatText
  "json" -> Right FormatJson
  "sarif" -> Right FormatSarif
  "github" -> Right FormatGithub
  other -> Left ("unknown format " <> other <> ", use text, json, sarif or github")

data FileResult = FileResult
  { frPath :: !FilePath,
    frSource :: !Text,
    frDiagnostics :: ![Diagnostic]
  }

severityWord :: Severity -> Text
severityWord = \case
  Error -> "error"
  Warn -> "warning"
  Info -> "info"

data Located = Located {startLine, startCol, endLine, endCol :: !Int}

locate :: FileResult -> Diagnostic -> Located
locate fr d =
  let idx = mkIndex (frSource fr)
      Span s e = diagSpan d
      (sl, sc) = toUtf16 idx s
      (el, ec) = toUtf16 idx e
   in Located (sl + 1) (sc + 1) (el + 1) (ec + 1)

renderResults :: Format -> [FileResult] -> Text
renderResults fmt results = case fmt of
  FormatText -> T.unlines [textLine fr d | fr <- results, d <- frDiagnostics fr]
  FormatGithub -> T.unlines [githubLine fr d | fr <- results, d <- frDiagnostics fr]
  FormatJson -> json (map jsonFile results)
  FormatSarif -> json (sarif results)
  where
    json v = decodeUtf8 (LBS.toStrict (encode v)) <> "\n"

textLine :: FileResult -> Diagnostic -> Text
textLine fr d =
  let Located {..} = locate fr d
      fixNote = if null (diagFixes d) then "" else " (fixable)"
   in T.pack (frPath fr) <> ":" <> showT startLine <> ":" <> showT startCol <> ": " <> severityWord (diagSeverity d) <> ": " <> diagMessage d <> " [" <> diagRule d <> "]" <> fixNote

githubLine :: FileResult -> Diagnostic -> Text
githubLine fr d =
  let Located {..} = locate fr d
      level = case diagSeverity d of
        Error -> "error"
        Warn -> "warning"
        Info -> "notice"
      props =
        T.intercalate
          ","
          [ "file=" <> prop (T.pack (frPath fr)),
            "line=" <> showT startLine,
            "col=" <> showT startCol,
            "endLine=" <> showT endLine,
            "endColumn=" <> showT endCol,
            "title=" <> prop ("korektor " <> diagRule d)
          ]
   in "::" <> level <> " " <> props <> "::" <> escapeData (diagMessage d)
  where
    escapeData = T.replace "\n" "%0A" . T.replace "\r" "%0D" . T.replace "%" "%25"
    prop = T.replace "," "%2C" . T.replace ":" "%3A" . escapeData

jsonFile :: FileResult -> Value
jsonFile fr =
  object
    [ "path" .= frPath fr,
      "diagnostics"
        .= [ object
               [ "rule" .= diagRule d,
                 "severity" .= diagSeverity d,
                 "message" .= diagMessage d,
                 "line" .= startLine,
                 "column" .= startCol,
                 "endLine" .= endLine,
                 "endColumn" .= endCol,
                 "span" .= diagSpan d,
                 "fixes" .= diagFixes d
               ]
           | d <- frDiagnostics fr,
             let Located {..} = locate fr d
           ]
    ]

sarif :: [FileResult] -> Value
sarif results =
  object
    [ "version" .= ("2.1.0" :: Text),
      "$schema" .= ("https://json.schemastore.org/sarif-2.1.0.json" :: Text),
      "runs"
        .= [ object
               [ "tool"
                   .= object
                     [ "driver"
                         .= object
                           [ "name" .= ("korektor" :: Text),
                             "informationUri" .= ("https://github.com/terrapkg/korektor" :: Text),
                             "rules" .= map ruleObject ruleIds
                           ]
                     ],
                 "results" .= [resultObject fr d | fr <- results, d <- frDiagnostics fr]
               ]
           ]
    ]
  where
    ruleIds = nub [diagRule d | fr <- results, d <- frDiagnostics fr]
    ruleObject rid =
      object
        [ "id" .= rid,
          "shortDescription" .= object ["text" .= maybe rid ruleDoc (findRule rid)]
        ]
    resultObject fr d =
      let Located {..} = locate fr d
       in object
            [ "ruleId" .= diagRule d,
              "level" .= case diagSeverity d of
                Error -> "error" :: Text
                Warn -> "warning"
                Info -> "note",
              "message" .= object ["text" .= diagMessage d],
              "locations"
                .= [ object
                       [ "physicalLocation"
                           .= object
                             [ "artifactLocation" .= object ["uri" .= frPath fr],
                               "region" .= object ["startLine" .= startLine, "startColumn" .= startCol, "endLine" .= endLine, "endColumn" .= endCol]
                             ]
                       ]
                   ]
            ]

showT :: Int -> Text
showT = T.pack . show
