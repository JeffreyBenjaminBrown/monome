module Monome.Util (
  ByteString
  , pack
  , unpack
  , fi, fr
  , numBetween
  , dot        -- ^ (Int,Int) -> (Int,Int) -> Int
  , taxiMetric -- ^ (Int,Int) -> (Int,Int) -> Int
  , addPair
  , negPair
  , uniq
  )

where

import qualified Data.Set as S
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)


-- | Because OSC needs a lot of Int32 values while I prefer Int.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fr :: Fractional a => Rational -> a
fr = fromRational

numBetween :: (Num a, Ord a) => a -> a -> a -> Bool
numBetween low high x = x >= low && x <= high

dot :: (Int,Int) -> (Int,Int) -> Int
dot (a,b) (c,d) = a*c + b*d

taxiMetric :: (Int,Int) -> (Int,Int) -> Int
taxiMetric (a,b) (c,d) = abs (a-c) + abs (b-d)

addPair :: (Int,Int) -> (Int,Int) -> (Int,Int)
addPair (a,b) (c,d) = (a+c, b+d)

negPair :: (Int,Int) -> (Int,Int)
negPair (a,b) = (-a,-b)

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList
