-----------------------------------------------------------------------------
-- |
-- Module      :  PersonalityGenerators
-- Copyright   :  (c) Sergey Lyashko 2023
-- License     :  see LICENSE
--
-- Generator for personality full names
-----------------------------------------------------------------------------

module PersonalityGenerators
  ( generation,
    fullNameGen,
    randomNumber
  )
where

import Data.Char (toUpper)
import PhoneGenerators
import System.Random

data FullName = FullName Name LastName deriving (Show)

type Name = String

type LastName = String

instance Generated FullName where
  toString (FullName name lastName) = capitalize name ++ " " ++ capitalize lastName

-- генерация случайного имени-фамилии
fullNameGen :: Int -> String
fullNameGen count =
  let nameLen = byRange 2 6 (randomNumber count) 1
      lastNameLen = byRange 2 9 (randomNumber (count + 1)) 1
   in generation (nameLen, lastNameLen)

byRange :: Int -> Int -> Int -> Int -> Int
byRange low high num i
 | num >= low && num <= high = num
 | otherwise = byRange low high (randomNumber num) (i + 1)

randomNumber :: Int -> Int
randomNumber offset =
  let numbers = randoms $ mkStdGen offset :: [Int]
      twoNumAsStr = take 1 . show $ map abs numbers !! 1
   in read twoNumAsStr :: Int

generation :: (Int, Int) -> String
generation (nameLen, lastNameLen) =
  let name = randomWord nameLen $ nameLen + lastNameLen
      lastName = randomWord lastNameLen $ lastNameLen * nameLen
   in toString $ FullName name lastName

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

randomWord :: Int -> Int -> String
randomWord charsCount offset = take charsCount (randomRs ('a', 'z') $ mkStdGen $ charsCount * offset)

-- пересечение всех имен и фамилий
cross :: [Name] -> [LastName] -> [String]
cross names lastNames = [toString (FullName name lastName :: FullName) | name <- names, lastName <- lastNames]
