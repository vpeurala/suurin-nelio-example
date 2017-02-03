module Lib
    (
     findBiggestSquare
   , generate100RandomBars
   , showRowOfBars
    ) where

import Control.Monad (forM, join, replicateM)
import System.Random (getStdRandom, randomR)

generate100RandomBars :: IO [Int]
generate100RandomBars =
  replicateM 100 (getStdRandom (randomR (1 :: Int, 6)))

findBiggestSquare :: [Int] -> [Int]
findBiggestSquare = id

showRowOfBars :: [Int] -> String
showRowOfBars = show