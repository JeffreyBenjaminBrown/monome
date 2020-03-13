module Monome.JiScales where

import Data.Fixed
import Data.List
import Data.Ratio

import Monome.Main
import Monome.Util


primes, subPrimes :: [Rational]
primes = [2,3,5,7,11,13,17,19,23,29,31]
subPrimes = map (1 /) $ tail primes

firstOctave, o1 :: (Ord a, Fractional a) => a -> a
firstOctave n =
  -- this would work for floats:
  -- firstOctave frac = 2 ** mod' (log frac / log 2) 1
  if n >= 2 then firstOctave $ n/2
  else if n >= 1 then n
  else if n <= 0 then error "rejected: negative number."
  else firstOctave $ n*2
o1 = firstOctave

x = sort $ o1 <$> reverse (take 6 subPrimes) ++ take 4 primes ++ [9]
y = sort $ o1 <$> [1,3,5,7,9]
go = do
  putStrLn $ show x
  putStrLn $ show y
  ji 14718 (fr <$> x) (fr <$> y)
