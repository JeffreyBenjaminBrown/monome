module Monome.Util (
  -- * from Data.ByteString
  ByteString
  , pack -- ^ String -> ByteString
  , unpack -- ^ ByteString -> String

  -- * defined here
  , fi         -- ^ (Integral a, Num b) => a -> b
  , numBetween -- ^ (Num a, Ord a) => a -> a -> a -> Bool
  , addPair -- ^ (Int,Int) -> (Int,Int) -> (Int,Int)
  , negPair -- ^ (Int,Int) -> (Int,Int)
  , nextVoice -- ^ M.Map VoiceId a -> VoiceId
  )

where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

import qualified Data.Map as M
import Monome.Types.Initial


-- | Because OSC needs a lot of Int32 values while I prefer Int.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

numBetween :: (Num a, Ord a) => a -> a -> a -> Bool
numBetween low high x = x >= low && x <= high

addPair :: (Int,Int) -> (Int,Int) -> (Int,Int)
addPair (a,b) (c,d) = (a+c, b+d)

negPair :: (Int,Int) -> (Int,Int)
negPair (a,b) = (-a,-b)

nextVoice :: M.Map VoiceId a -> VoiceId
nextVoice m =
  case M.lookupMax m of
    Nothing -> (0,0)
    Just ((x,_),_) -> (x+1,0)
    -- Produces something bigger than any key in `m`,
    -- without looking up the snd in the pair.
    -- Note that (0,1) < (1,0).
