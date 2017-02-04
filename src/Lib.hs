module Lib
    (
     findBiggestSquare
   , generate100RandomBars
   , showRowOfBars
    ) where

import Control.Monad (forM, join, replicateM)
import Data.List (intercalate)
import System.Random (getStdRandom, randomR)

generate100RandomBars :: IO [Int]
generate100RandomBars =
  replicateM 100 (getStdRandom (randomR (1 :: Int, 6)))

findBiggestSquare :: [Int] -> [Int]
findBiggestSquare = id

showRowOfBars :: [Int] -> String
showRowOfBars bars =
  let minBar = minimum bars
      maxBar = maximum bars
      length' = length bars
      range = enumFromThenTo maxBar (maxBar - 1) minBar
      meter :: [String]
      meter = fmap (\height -> row height) range
      row :: Int -> String
      row height = fmap (\bar -> mark bar height) range
      mark :: Int -> Int -> Char
      mark bar height = if (bar >= height) then '*' else ' '
  in show (range, intercalate "\n" meter, length')