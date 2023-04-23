module Output
  ( printOnConsole,
    printToFile,
  )
where

import Data.Time
import PhoneGenerators

-- генерация случайных номеров с выводом в файл
printToFile :: PhonePrefix -> Amount -> FilePath -> IO ()
printToFile pref amount path =
  let toFileFunc = toFile path
   in printRandomPhones pref amount toFileFunc

-- генерация случайных номеров с выводом на консоль
printOnConsole :: PhonePrefix -> Amount -> IO ()
printOnConsole pref amount = printRandomPhones pref amount onConsole

printRandomPhones :: PhonePrefix -> Amount -> ([String] -> IO ()) -> IO ()
printRandomPhones pref amount ioFunc = do
  time <- getCurrentTime
  let offset = takeWhile (/= ' ') $ drop 20 $ show time
  ioFunc $ randomPhoneGen pref amount (read offset :: Int)

onConsole :: [String] -> IO ()
onConsole [] = putStrLn ""
onConsole (x : xs) = do
  putStrLn x
  onConsole xs

toFile :: FilePath -> [String] -> IO ()
toFile fileName [] = appendFile fileName ""
toFile fileName (x : xs) = do
  appendFile fileName (x ++ "\n")
  toFile fileName xs
