module Lib
    ( generate100RandomBars
    ) where

import Control.Monad (forM, join, replicateM)
import System.Random (getStdRandom, randomR)

generate100RandomBars :: IO [Int]
generate100RandomBars =
  replicateM 100 (getStdRandom (randomR (1 :: Int, 6)))
