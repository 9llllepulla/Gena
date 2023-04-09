-----------------------------------------------------------------------------
-- |
-- Module      :  PersonalityGenerators
-- Copyright   :  (c) Sergey Lyashko 2023
-- License     :  see LICENSE
--
-- Generator for personality full names
-----------------------------------------------------------------------------

module PersonalityGenerators (fullNamesGen) where

import Data.Char (toUpper)
import PhoneGenerators
import System.Random

type Name = String

type LastName = String

type Amount = Int

data FullName = FullName Name LastName deriving (Show)

instance Generated FullName where
  toString (FullName name lastName) = capitalize name ++ " " ++ capitalize lastName

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

-- генерация списка уникальных случайных имени-фамилии
fullNamesGen :: Amount -> [String]
fullNamesGen amount = uniqueFilter . map (toString . nameGen) $ take amount [1 ..]

uniqueFilter :: [String] -> [String]
uniqueFilter [] = []
uniqueFilter (x : xs) = x : uniqueFilter (filter (/= x) xs)

nameGen :: Int -> FullName
nameGen offset =
  let nameLen = byRange 2 6 (randomNumber offset) 1
      lastNameLen = byRange 2 9 (randomNumber (offset + 1)) 1
   in generationFullName (nameLen, lastNameLen)

byRange :: Int -> Int -> Int -> Int -> Int
byRange low high num i
 | num >= low && num <= high = num
 | otherwise = byRange low high (randomNumber num) (i + 1)

randomNumber :: Int -> Int
randomNumber offset =
  let numbers = randoms $ mkStdGen offset :: [Int]
      twoNumAsStr = take 1 . show $ map abs numbers !! offset
   in read twoNumAsStr :: Int

generationFullName :: (Int, Int) -> FullName
generationFullName (nameLen, lastNameLen) =
  let name = randomAnyName nameLen $ nameLen + lastNameLen
      lastName = randomAnyName lastNameLen $ lastNameLen * nameLen
   in FullName name lastName

randomAnyName :: Int -> Int -> String
randomAnyName charsCount offset = take charsCount (randomRs ('a', 'z') $ mkStdGen $ charsCount * offset)
