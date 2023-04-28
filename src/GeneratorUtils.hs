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
    randomNumbers,
    Amount,
    Offset
  )
where
  
import System.Random
  
type Offset = Int

type Amount = Int  

uniqueFilter :: (Eq a) => [a] -> [a]
uniqueFilter [] = []
uniqueFilter (x : xs) = x : uniqueFilter (filter (/= x) xs)

randomNumbers :: Offset -> Amount -> [Int]
randomNumbers offset amount =
  let generated = randoms $ mkStdGen offset :: [Int]
   in map abs $ take amount generated