module Output
  ( saveToFile,
    printRandomPhones
  )
where

import Data.Time
import PhoneGenerators

-- генерация случайных номеров с выводом на консоль
printRandomPhones :: PhonePrefix -> Amount -> IO ()
printRandomPhones pref amount = do
  time <- getCurrentTime
  let offset = takeWhile (/= ' ') $ drop 20 $ show time
  printArray $ randomPhoneGen pref amount (read offset :: Int)

printArray :: [String] -> IO ()
printArray [] = putStrLn ""
printArray (x : xs) = do
  putStrLn x
  printArray xs

saveToFile :: FilePath -> [String] -> IO ()
saveToFile fileName [] = appendFile fileName ""
saveToFile fileName (x : xs) = do
  appendFile fileName (x ++ "\n")
  saveToFile fileName xs
