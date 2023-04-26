module CommandLine (commandArgs) where

commandArgs :: [String] -> String
commandArgs [] = help
commandArgs (x : xs)
  | x == "-a" = "(c) tg: @gILLLepuLLa"
  | x == "phone" = phone xs
  | x == "fio" = fullNames xs
  | otherwise = commandArgs xs

fullNames :: [String] -> String
fullNames (x : xs) = undefined

phone :: [String] -> String
phone (x : xs) = undefined

help :: String
help = 
 "=============================" ++ "\n" ++ 
 "[ Data generator ] > Help <" ++ "\n" ++ 
 "=============================" ++ "\n" ++ 
 "------------------------------------------------------------------------------------------------------------" ++ "\n" ++
 "Генерация номеров телефонов: --> |> phone {prefix - число} {amount - число} {-r - random} {file - имя файла}" ++ "\n" ++
 "---------------------------" ++ "\n" ++
 "|> phone 7 3                   -- вывод заданного количества номеров с перефиксом" ++ "\n" ++        
 "|> phone 7 3 \"test.txt\"        -- вывод в файл заданного количества номеров с перефиксом"++ "\n" ++ 
 "|> phone 7 3 -r \"test.txt\"     -- вывод в файл заданного количества случайных номеров с перефиксом"++ "\n" ++ 
 "------------------------------------------------------------------------------------------------------------" ++ "\n" ++
 "Генерация ФИО: --> |> fio {amount - число} {file - имя файла}"  ++ "\n" ++
 "--------------------------" ++ "\n" ++
 "|> fio 10                   -- вывод заданного количества случайных \"Фамилия,Имя,Отчество\" "++ "\n" ++
 "|> fio 10 \"test.txt\"        -- вывод в файл заданного количества случайных \"Фамилия,Имя,Отчество\" "++ "\n" ++
 "------------------------------------------------------------------------------------------------------------"