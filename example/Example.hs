module Main where

import CCCEdict qualified
import Control.Category ((>>>))
import Data.Char (isAscii, isNumber, isSpace)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as M
import Data.Ord (Down (Down))
import Data.Text qualified as T
import Streaming.Prelude qualified as S

isMultiLingual :: T.Text -> Bool
isMultiLingual =
  T.takeWhile (isSpace >>> not)
    >>> T.any (\c -> isAscii c || isNumber c)

top10 :: M.Map k Int -> [(k, Int)]
top10 =
  M.toList
    >>> List.sortOn (snd >>> Down)
    >>> take 10

reportLine :: Char -> Int -> String
reportLine c n = c : "\t" <> show n

main :: IO ()
main =
  S.stdinLn
    & S.map T.pack
    & CCCEdict.parseLines
    & S.map CCCEdict.defTrad
    & S.filter (isMultiLingual >>> not)
    & flip S.for (T.unpack >>> S.each)
    & S.fold_ (\p c -> M.insertWith (+) c (1 :: Int) p) mempty id
    & fmap (top10 >>> fmap (uncurry reportLine) >>> S.each)
    & (>>= S.stdoutLn)
