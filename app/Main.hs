module Main where

import Lib

main :: IO ()
main = do
  randomBars <- generate100RandomBars
  let biggestSquare :: [Int]
      biggestSquare = findBiggestSquare randomBars
  putStrLn (showRowOfBars randomBars)
  putStrLn (showRowOfBars biggestSquare)


