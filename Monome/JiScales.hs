module Monome.JiScales where

import Data.List

import Monome.Main
import Monome.Types.Initial
import Monome.Util


go :: IO (St JiApp)
go = do
  putStrLn $ show gen
  putStrLn $ show shf
  ji 14718 (fr <$> gen) (fr <$> shf)

gen, shf :: [Rational]
gen = sort . uniq $ o1 <$> [1..15]
shf = sort . uniq $ o1 <$> [1..9]
-- o1 <$> [1, 3, 7, 21]
-- [1,4/3,3,9]
-- x = sort $ o1 <$> ( takeWhile (<= 7) primes
--                     ++ takeWhile (>= (1%7)) subPrimes )

primes, subPrimes :: [Rational]
primes = [2,3,5,7,11,13,17,19,23,29,31]
subPrimes = map (1 /) $ tail primes

o1, firstOctave :: (Ord a, Fractional a) => a -> a
o1 = firstOctave
firstOctave n =
  -- this would work for floats:
  -- firstOctave frac = 2 ** mod' (log frac / log 2) 1
  if n >= 2 then firstOctave $ n/2
  else if n >= 1 then n
  else if n <= 0 then error "rejected: negative number."
  else firstOctave $ n*2
