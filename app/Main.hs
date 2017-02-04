module Main where

import Lib

main :: IO ()
main = do
  randomBars <- generate100RandomBars
  let biggestSquare :: Int
      biggestSquare = findBiggestSquareSize randomBars
  putStrLn $ show randomBars
  putStrLn (showRowOfBars randomBars)
  putStrLn (show biggestSquare)
  --putStrLn (showRowOfBars biggestSquare)


