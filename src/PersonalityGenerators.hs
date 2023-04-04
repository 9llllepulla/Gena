module PersonalityGenerators
  ( cross,
    randomFullNameGen,
  )
where

import Data.Char (toUpper)
import Generators
import System.Random

data FullName = FullName Name LastName deriving (Show)

type Name = String

type LastName = String

instance Generated FullName where
  toString (FullName name lastName) = capitalize name ++ " " ++ capitalize lastName

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

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
