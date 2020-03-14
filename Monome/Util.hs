module Monome.Util (
  ByteString
  , pack
  , unpack
  , fi, fr
  , numBetween
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

addPair :: (Int,Int) -> (Int,Int) -> (Int,Int)
addPair (a,b) (c,d) = (a+c, b+d)

negPair :: (Int,Int) -> (Int,Int)
negPair (a,b) = (-a,-b)

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList
