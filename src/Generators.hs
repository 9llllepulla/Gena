module Generators
  ( phonesGen,
    randomPhoneGen,
  )
where

import System.Random

type PhonePrefix = Int

type Name = String

type LastName = String

-- генератор заданного количества случайных номеров телефонов с префиксом
randomPhoneGen :: PhonePrefix -> Int -> [String]
randomPhoneGen prefix cnt =
  let phones = map abs $ random' prefix cnt
      digits = phones `toStringByPrefix` prefix
   in map (take 10) digits

-- генератор заданного количества номеров телефонов по префиксу
phonesGen :: PhonePrefix -> Int -> [String]
phonesGen prefix count = toStringByPrefix (take count [100000000 ..]) prefix

random' :: PhonePrefix -> Int -> [Int]
random' p n =
  let num = 31 * n + p
  in take n (randoms $ mkStdGen num :: [Int])

-- пересечение всех имен и фамилий
cross :: [Name] -> [LastName] -> [String]
cross names lastNames = [prefix ++ " " ++ name | name <- names, prefix <- lastNames]

toStringByPrefix :: (Show a) => [a] -> Int -> [String]
toStringByPrefix arr p = map (\x -> show p ++ show x) arr
