-----------------------------------------------------------------------------
-- |
-- Module      :  PersonalityGenerators
-- Copyright   :  (c) Sergey Lyashko 2023
-- License     :  see LICENSE
--
-- Generator for personality full names
--
-----------------------------------------------------------------------------
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

-- генерация случайного имени-фамилии
randomFullNameGen :: (Int, Int) -> Int -> [String]
randomFullNameGen (nameLen, lastNameLen) offset =
  let name = randomName nameLen $ nameLen * offset
      lastName = randomName lastNameLen $ lastNameLen * offset + nameLen
   in cross [name] [lastName]

randomName :: Int -> Int -> String
randomName charsCount offset = take charsCount (randomRs ('a', 'z') $ mkStdGen $ charsCount * offset)

-- пересечение всех имен и фамилий
cross :: [Name] -> [LastName] -> [String]
cross names lastNames = [toString (FullName name lastName :: FullName) | name <- names, lastName <- lastNames]
