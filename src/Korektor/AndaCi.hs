module Korektor.AndaCi
  ( parseAndaCi,
    andaCiDirs,
    readAndaCi,
  )
where

import Data.Aeson (FromJSON (..), eitherDecodeStrict, withObject, (.:))
import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory)

newtype BuildEntry = BuildEntry Text

instance FromJSON BuildEntry where
  parseJSON = withObject "build entry" (fmap BuildEntry . (.: "pkg"))

parseAndaCi :: Text -> Either Text [Text]
parseAndaCi input =
  case eitherDecodeStrict (encodeUtf8 matrix) of
    Right entries -> Right (nub [pkg | BuildEntry pkg <- entries])
    Left err -> Left ("can't read anda ci output: " <> T.pack err)
  where
    stripped = T.strip input
    matrix = case [rest | l <- T.lines stripped, Just rest <- [T.stripPrefix "build_matrix=" (T.strip l)]] of
      found : _ -> found
      []
        | T.null stripped -> "[]"
        | otherwise -> stripped

andaCiDirs :: [Text] -> IO [FilePath]
andaCiDirs pkgs = nub <$> mapM projectDir pkgs
  where
    projectDir pkg = do
      let path = T.unpack pkg
      isDir <- doesDirectoryExist path
      pure (if isDir then path else takeDirectory path)

readAndaCi :: FilePath -> IO (Either Text [FilePath])
readAndaCi source = do
  bytes <- if source == "-" then BS.getContents else BS.readFile source
  either (pure . Left) (fmap Right . andaCiDirs) (parseAndaCi (decodeUtf8Lenient bytes))
