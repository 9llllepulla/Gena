module Main (main) where

import PhoneGenerators
import PersonalityGenerators

main :: IO ()
main = do
  print $ phonesGen 7 3
  print $ randomPhoneGen 7 3
  print $ generation (6, 8)
  print $ randomNumber 3
  print $ fullNameGen 5

