-----------------------------------------------------------------------------
-- |
-- Module      :  Generators
-- Copyright   :  (c) Sergey Lyashko 2023
-- License     :  see LICENSE
--
-- Generator for phone numbers by prefix
--
-----------------------------------------------------------------------------
module PhoneGenerators
  ( phonesGen,
    randomPhoneGen,
    PhonePrefix,
    Amount,
    Offset,
    Generated (..)
  )
where

import GeneratorUtils
import System.Random

class Generated a where
  toString :: a -> String

instance Generated PhoneNumber where
  toString (PhoneNumber prefix number) = show prefix ++ show number

data PhoneNumber = PhoneNumber PhonePrefix Int deriving (Show)

type PhonePrefix = Int

type Offset = Int

type Amount = Int

-- генератор заданного количества уникальных случайных номеров телефонов с префиксом по коэффициенту смещения
randomPhoneGen :: PhonePrefix -> Amount -> Offset -> [String]
randomPhoneGen prefix amount offset = 
  let nums = randomNumbers offset amount
   in take amount $ uniqueFilter $ map (take 11 . phoneWith prefix) nums

-- генератор заданного количества номеров телефонов по префиксу
phonesGen :: PhonePrefix -> Amount -> [String]
phonesGen prefix amount = map (phoneWith prefix) $ take amount [100000000 ..]

phoneWith :: PhonePrefix -> Int -> String
phoneWith prefix = toString . PhoneNumber prefix

randomNumbers :: Int -> Amount -> [Int]
randomNumbers offset amount =
  let generated = randoms $ mkStdGen offset :: [Int]
   in map abs $ take (amount + 10) generated
