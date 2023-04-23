module Main (main) where

import PhoneOutput

main :: IO ()
main = do
  printRandomPhonesOnConsole 7 3
  printRandomPhonesToFile 7 3 "3_mobile.txt"
