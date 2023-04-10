-----------------------------------------------------------------------------
-- |
-- Module      :  GeneratorUtils
-- Copyright   :  (c) Sergey Lyashko 2023
-- License     :  see LICENSE
--
-- Util methods for any generators
-----------------------------------------------------------------------------

module GeneratorUtils
  ( uniqueFilter,
    Amount,
  )
where

type Amount = Int

uniqueFilter :: (Eq a) => [a] -> [a]
uniqueFilter [] = []
uniqueFilter (x : xs) = x : uniqueFilter (filter (/= x) xs)
