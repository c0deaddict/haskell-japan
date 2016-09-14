module Main where

import           Lib

main :: IO ()
main =
  do
    specStr <- getContents
    solveAndPrint (readSpec specStr)
  where
    readSpec :: String -> ([Spec], [Spec])
    readSpec = read
