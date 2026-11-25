module Korektor.Position
  ( LineIndex,
    mkIndex,
    lineCount,
    lineAt,
    utf16Col,
    toUtf16,
    fromUtf16,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Syntax (Pos (..))

data LineIndex = LineIndex !(IntMap (Int, Text)) !Int

mkIndex :: Text -> LineIndex
mkIndex src =
  let ls = T.splitOn "\n" src
      starts = scanl (\o l -> o + T.length l + 1) 0 ls
   in LineIndex (IntMap.fromList (zip [0 ..] (zip starts ls))) (length ls)

lineCount :: LineIndex -> Int
lineCount (LineIndex _ n) = n

lineAt :: LineIndex -> Int -> Maybe (Int, Text)
lineAt (LineIndex m _) l = IntMap.lookup l m

units :: Char -> Int
units c = if fromEnum c > 0xFFFF then 2 else 1

utf16Col :: Text -> Int -> Int
utf16Col lineText col = T.foldl' (\n c -> n + units c) 0 (T.take col lineText)

toUtf16 :: LineIndex -> Pos -> (Int, Int)
toUtf16 idx (Pos _ l c) = case lineAt idx l of
  Just (_, t) -> (l, utf16Col t c)
  Nothing -> (l, c)

fromUtf16 :: LineIndex -> Int -> Int -> Pos
fromUtf16 idx l ch = case lineAt idx l of
  Just (start, t) ->
    let col = go 0 0 (T.unpack t)
        go n u = \case
          c : cs | u + units c <= ch -> go (n + 1) (u + units c) cs
          _ -> n
     in Pos (start + col) l col
  Nothing -> Pos 0 l ch
