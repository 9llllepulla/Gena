-----------------------------------------------------------------------------
-- |
-- Module      :  Generators
-- Copyright   :  (c) Sergey Lyashko 2023
-- License     :  see LICENSE
--
-- Generator for phone numbers by prefix
--
-----------------------------------------------------------------------------
module Generators
  ( phonesGen,
    randomPhoneGen,
    Generated (..)
  )
where

import System.Random

class Generated a where
  toString :: a -> String

instance Generated PhoneNumber where
  toString (PhoneNumber prefix number) = show prefix ++ show number

data PhoneNumber = PhoneNumber PhonePrefix Int deriving (Show)

type PhonePrefix = Int

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
