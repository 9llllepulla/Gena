module Generators
  ( phonesGen,
    randomPhoneGen,
  )
where

import System.Random

class Generated a where
  toString :: a -> String

instance Generated PhoneNumber where
  toString (PhoneNumber prefix number) = show prefix ++ show number

data PhoneNumber = PhoneNumber PhonePrefix Int deriving (Show)

type PhonePrefix = Int

type Name = String

type LastName = String

-- генератор заданного количества случайных номеров телефонов с префиксом
randomPhoneGen :: PhonePrefix -> Int -> [String]
randomPhoneGen prefix = map (take 10 . phoneWith prefix) . rndBy prefix

-- генератор заданного количества номеров телефонов по префиксу
phonesGen :: PhonePrefix -> Int -> [String]
phonesGen prefix count = map (phoneWith prefix) $ take count [100000000 ..]

phoneWith :: PhonePrefix -> Int -> String
phoneWith prefix = toString . PhoneNumber prefix

rndBy :: PhonePrefix -> Int -> [Int]
rndBy prefix count =
  let generated = randoms $ mkStdGen (prefix * count) :: [Int]
   in map abs $ take count generated

-- пересечение всех имен и фамилий
cross :: [Name] -> [LastName] -> [String]
cross names lastNames = [name ++ " " ++ lastName | name <- names, lastName <- lastNames]
