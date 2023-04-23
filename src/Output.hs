module Output
  ( printRandomPhonesOnConsole,
    printRandomPhonesToFile,
    printOrderedPhonesToConsole
  )
where

import Data.Time
import PhoneGenerators

-- генерация упорядоченных номеров с выводом на консоль
printOrderedPhonesToConsole :: PhonePrefix -> Amount -> IO ()
printOrderedPhonesToConsole pref amount = onConsole $ orderedPhonesGen pref amount


-- генерация случайных номеров с выводом в файл
printRandomPhonesToFile :: PhonePrefix -> Amount -> FilePath -> IO ()
printRandomPhonesToFile pref amount path =
  let toFileFunc = toFile path
   in printRandomPhones pref amount toFileFunc

-- генерация случайных номеров с выводом на консоль
printRandomPhonesOnConsole :: PhonePrefix -> Amount -> IO ()
printRandomPhonesOnConsole pref amount = printRandomPhones pref amount onConsole

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
