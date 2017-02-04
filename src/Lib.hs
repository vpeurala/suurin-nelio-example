module Lib
    (
     findBiggestSquareSize
   , generate100RandomBars
   , showRowOfBars
    ) where

import Control.Monad (forM, join, replicateM)
import Data.List (all, intercalate)
import Debug.Trace (trace)
import System.Random (getStdRandom, randomR)

generate100RandomBars :: IO [Int]
generate100RandomBars =
  replicateM 100 (getStdRandom (randomR (1 :: Int, 6)))

-- The width and height (which are the same number) of a square,
-- represented as a single number here.
type SquareSize = Int

type Bar = Int

findBiggestSquareSize :: [Bar] -> SquareSize
findBiggestSquareSize bars =
  let minBar = minimum bars
      maxBar = maximum bars
      length' = length bars
      -- The size of square we start looking for first is the
      -- minimum of the largest bar and the length, since
      -- a square contained in a rectangle with width = length'
      -- and height = maxBar cannot be larger than the smaller of
      -- the dimensions.
      startingAttempt = min maxBar length'
  in  iterativelyFindBiggestSquareSize bars startingAttempt

iterativelyFindBiggestSquareSize :: [Bar] -> SquareSize -> SquareSize
iterativelyFindBiggestSquareSize _ 1 = 1
iterativelyFindBiggestSquareSize bars attemptedSize =
  trace ("attempt size " ++ (show attemptedSize))
  (if (containsSquareOfSize bars attemptedSize)
  then attemptedSize
  else iterativelyFindBiggestSquareSize bars (attemptedSize - 1))

containsSquareOfSize :: [Bar] -> SquareSize -> Bool
containsSquareOfSize _ 1 = True
containsSquareOfSize bars size | length bars < size = False
containsSquareOfSize bars size =
  let window = take size bars
  in  trace (show window) (all (\b -> b >= size) window)

showRowOfBars :: [Bar] -> String
showRowOfBars bars =
  let minBar = minimum bars
      maxBar = maximum bars
      length' = length bars
      range = enumFromThenTo maxBar (maxBar - 1) minBar
      meter :: [String]
      meter = fmap (\height -> row height) range
      row :: Int -> String
      row height = fmap (\bar -> mark bar height) bars
      mark :: Int -> Int -> Char
      mark bar height = if (bar >= height) then '*' else ' '
  in intercalate "\n" meter