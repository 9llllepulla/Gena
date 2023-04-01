module Generators
  ( toFile,
    phoneGen,
    printArray
  )
where

namesGen :: [String] -> [String] -> [String]
namesGen names namePrefix = [prefix ++ " " ++ name | name <- names, prefix <- namePrefix]

phoneGen :: Int -> Int -> [String]
phoneGen prefix count = map (\x -> show prefix ++ show x) (take count [100000000 ..])

printArray :: [String] -> IO ()
printArray [] = putStrLn ""
printArray (x : xs) = do
  putStrLn x
  printArray xs

toFile :: FilePath -> [String] -> IO ()
toFile fileName [] = appendFile fileName ""
toFile fileName (x : xs) = do
  appendFile fileName (x ++ "\n")
  toFile fileName xs
