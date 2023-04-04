module Generators
  ( phonesGen,
    randomPhoneGen,
    cross,
    randomFullNameGen,
  )
where

import Data.Char (toUpper)
import System.Random

class Generated a where
  toString :: a -> String

instance Generated PhoneNumber where
  toString (PhoneNumber prefix number) = show prefix ++ show number

data PhoneNumber = PhoneNumber PhonePrefix Int deriving (Show)

type PhonePrefix = Int

data FullName = FullName Name LastName deriving (Show)

type Name = String

type LastName = String

instance Generated FullName where
  toString (FullName name lastName) = capitalize name ++ " " ++ capitalize lastName

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

-- генератор заданного количества случайных номеров телефонов с префиксом
randomPhoneGen :: PhonePrefix -> Int -> [String]
randomPhoneGen prefix = map (take 10 . phoneWith prefix) . randomNumbers prefix

-- генератор заданного количества номеров телефонов по префиксу
phonesGen :: PhonePrefix -> Int -> [String]
phonesGen prefix count = map (phoneWith prefix) $ take count [100000000 ..]

phoneWith :: PhonePrefix -> Int -> String
phoneWith prefix = toString . PhoneNumber prefix

randomNumbers :: PhonePrefix -> Int -> [Int]
randomNumbers prefix count =
  let generated = randoms $ mkStdGen (prefix * count) :: [Int]
   in map abs $ take count generated

randomFullNameGen :: (Int, Int) -> Int -> [String]
randomFullNameGen (nameLen, lastNameLen) count =
  let name = randomName nameLen $ nameLen * count
      lastName = randomName lastNameLen $ lastNameLen * count + nameLen
   in cross [name] [lastName]

randomName :: Int -> Int -> String
randomName count offset = take count (randomRs ('a', 'z') $ mkStdGen $ count * offset)

-- пересечение всех имен и фамилий
cross :: [Name] -> [LastName] -> [String]
--cross names lastNames = [name ++ " " ++ lastName | name <- names, lastName <- lastNames]
cross names lastNames = [toString (FullName name lastName :: FullName) | name <- names, lastName <- lastNames]
