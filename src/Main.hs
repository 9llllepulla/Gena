module Main (main) where

import Generators
import PersonalityGenerators

main :: IO ()
main = do
  print $ phonesGen 7 3
  print $ randomPhoneGen 7 3
  print $ randomFullNameGen (6, 8) 3
