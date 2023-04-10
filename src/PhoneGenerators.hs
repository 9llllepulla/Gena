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

-- генератор заданного количества уникальных случайных номеров телефонов с префиксом
randomPhoneGen :: PhonePrefix -> Amount -> [String]
randomPhoneGen prefix amount = 
  let nums = randomNumbers prefix amount
      numsAsStr = map (take 10 . phoneWith prefix) nums
   in take amount $ uniqueFilter numsAsStr

-- генератор заданного количества номеров телефонов по префиксу
phonesGen :: PhonePrefix -> Amount -> [String]
phonesGen prefix amount = map (phoneWith prefix) $ take amount [100000000 ..]

phoneWith :: PhonePrefix -> Int -> String
phoneWith prefix = toString . PhoneNumber prefix

randomNumbers :: PhonePrefix -> Amount -> [Int]
randomNumbers prefix amount =
  let generated = randoms $ mkStdGen (prefix * amount) :: [Int]
   in map abs $ take (amount + 10) generated
