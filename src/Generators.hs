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
  let digits = map abs $ rnd prefix count
   in map (take 10) $ digits `toStringWithPrefix` prefix

-- генератор заданного количества номеров телефонов по префиксу
phonesGen :: PhonePrefix -> Int -> [String]
phonesGen prefix count = take count [100000000 ..] `toStringWithPrefix` prefix

rnd :: PhonePrefix -> Int -> [Int]
rnd prefix count = take count (randoms $ mkStdGen (prefix * count) :: [Int])

toStringWithPrefix :: (Show a) => [a] -> a -> [String]
toStringWithPrefix arr prefix = map (\x -> show prefix ++ show x) arr

-- пересечение всех имен и фамилий
cross :: [Name] -> [LastName] -> [String]
cross names lastNames = [name ++ " " ++ lastName | name <- names, lastName <- lastNames]
