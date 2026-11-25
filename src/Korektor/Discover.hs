module Korektor.Discover (findSpecs) where

import Control.Monad (filterM, forM)
import Data.List (isSuffixOf, sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

findSpecs :: [FilePath] -> IO (Either FilePath [FilePath])
findSpecs paths = do
  found <- forM paths $ \p -> do
    isFile <- doesFileExist p
    isDir <- doesDirectoryExist p
    if
      | isFile -> pure (Right [p])
      | isDir -> Right <$> walk p
      | otherwise -> pure (Left p)
  pure (concat <$> sequence found)

walk :: FilePath -> IO [FilePath]
walk dir = do
  entries <- sort . filter (not . skipped) <$> listDirectory dir
  let full = map (dir </>) entries
  files <- filterM doesFileExist full
  dirs <- filterM doesDirectoryExist full
  nested <- concat <$> mapM walk dirs
  pure (filter (".spec" `isSuffixOf`) files <> nested)
  where
    skipped e = e `elem` [".git", "dist-newstyle", "node_modules", "anda-build", "target"]
