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
randomPhoneGen prefix count =
  let numbers = map abs $ rnd prefix count
   in map (take 10) $ numbers `toStringByPrefix` prefix

-- генератор заданного количества номеров телефонов по префиксу
phonesGen :: PhonePrefix -> Int -> [String]
phonesGen prefix count = take count [100000000 ..] `toStringByPrefix` prefix

rnd :: PhonePrefix -> Int -> [Int]
rnd prefix count = take count (randoms $ mkStdGen (prefix * count) :: [Int])

-- пересечение всех имен и фамилий
cross :: [Name] -> [LastName] -> [String]
cross names lastNames = [prefix ++ " " ++ name | name <- names, prefix <- lastNames]

toStringByPrefix :: (Show a) => [a] -> Int -> [String]
toStringByPrefix arr p = map (\x -> show p ++ show x) arr
