module Output
  ( saveToFile,
    printArray,
  )
where

saveToFile :: FilePath -> [String] -> IO ()
saveToFile fileName [] = appendFile fileName ""
saveToFile fileName (x : xs) = do
  appendFile fileName (x ++ "\n")
  saveToFile fileName xs

printArray :: [String] -> IO ()
printArray [] = putStrLn ""
printArray (x : xs) = do
  putStrLn x
  printArray xs
