{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables
, TypeApplications #-}

module Monome.JiScales where

import Prelude hiding (lines)

import Data.List hiding (lines)
import Data.Ratio

import Monome.Main
import Monome.Types.Initial
import Monome.Util


go :: [Rational] -> [Rational] -> IO (St JiApp)
go g h = do
  showGrid g h 1
  ji 14718 (fr <$> g) (fr <$> h)

g1, g2 :: [Rational]
g1 = sort $ fmap firstOctave diatonic
g2 = sort $ fmap firstOctave star

sev, sev4 :: [Rational]
sev = sort . map o1 $ [1,9,8/7,7/6,21,3,49,7]
sev4 = sort . map o1 $ [1,21,3,7]

diatonic, minor, major, star :: [Rational]
diatonic = sort . map o1 $ 10%9 : major
  -- Union of major and minor, appropriately transposed.
  -- Major and minor are not transpositions of each other. If C = 1, then
  -- in the C-major scale, the fifth from G to D is perfect,
  -- while in the A-minor scale, the fifth from D to A is.
  -- That is, the major scale has a howling Dorian mode,
  -- while the minor scale has a howling Mixolydian.
minor = sort . map o1 $ [1, 9, 6/5, 4/3, 3, 8/5, 9/5]
major = sort . map o1 $ [1,9,5,4%3,3,5%3,15]
star =  sort . map o1 $ [1,3,5,7,15,21]
  -- I like this as the second generator.
  -- It's not quite a cube -- it's missing 5*7 and 3*5*7.
  -- I omit 35 because there are already so many different whole steps,
  -- and 3*5*7 because it's too complex to be worth it.

-- | to see every note generated by the two generators
cross :: (Fractional a, Ord a) => [a] -> [a] -> [a]
cross gen1 gen2 =
  sort . uniq . map firstOctave
  $ map (*) gen1 <*> gen2

-- | every interval in a scale
intervals :: (Fractional a, Ord a) => [a] -> [a]
intervals gen =
  sort . uniq . map firstOctave
  $ map (/) gen <*> gen

-- The intervals generated by both the major and the minor scale are
-- the same. There are 59 of them:
-- > cmi = cross minor minor
-- > cma = cross major major
-- > intervals cmi == intervals cma
-- True
-- > length $ uniq $ intervals cmi
-- 59
-- The diatonic scale generates another 66 more,
-- but I expect to ignore them.

showLabeled :: [Rational] -> IO ()
showLabeled =
  mapM_ (putStrLn . show)
  . zip (map show [(1::Int) ..])
  . map (\x -> ( pad showLen $ show x
               , pad showLen ( take 3
                               $ show (fr x :: Float))))

showGrid :: [Rational] -> [Rational] -> Rational -> IO ()
showGrid g h center = let
  norm :: Rational -> Rational -> Rational
  norm h1 = o1 . (* h1) . (/ center)
  showFrac, showDoDecimal :: Rational -> String
  showFrac =      pad showLen .          show
  showDoDecimal = pad showLen . take 5 . show .
                  round . (*) 1200 . (\x -> log x / log 2) . fr
  grid :: [[Rational]] =
    map (\h1 -> map (norm h1) g) h
  showRow :: [Rational] -> IO ()
  showRow rs = mapM_ (putStr . showFrac   ) rs   >> putStrLn "" >>
               mapM_ (putStr . showDoDecimal) rs >> putStrLn ""
  in mapM_ showRow grid

-- confusing
--g1 = sort . uniq $ firstOctave <$> [1..15]
--g2 = sort . uniq $ firstOctave <$> [1..9]

-- another idiom
-- x = sort $ firstOctave <$> ( filter (<= 7) primes
--                              ++ filter (>= (1%7)) subPrimes )

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

pad :: Int -> String -> String
pad n s = case length s < n of
  False -> s ++ "!!"
  True -> s ++ replicate (max (n - length s) 0) ' '

showLen :: Int
showLen = 10
