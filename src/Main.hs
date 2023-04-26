module Main (main) where

import System.Environment
import CommandLine

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ commandArgs args

{-main :: IO ()
  main = do
    printRandomPhonesOnConsole 7 3
    printRandomPhonesToFile 7 3 "3_mobile.txt"
    -}
