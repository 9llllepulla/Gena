module Main (main) where

import Generators

main :: IO ()
main = do
  print $ phonesGen 7 3
  print $ randomPhoneGen 7 3
