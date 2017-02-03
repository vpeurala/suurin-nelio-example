module Main where

import Lib

main :: IO ()
main = generate100RandomBars >>= (putStrLn . show)
