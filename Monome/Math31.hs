{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, OverloadedStrings
, TupleSections #-}

module Monome.Math31 (
  Pitch, PitchClass, LitPitches
  , et31ToFreq      -- ^ Pitch -> Float
  , xyToEt31        -- ^ (X,Y) -> Pitch
  , et31ToLowXY     -- ^ PitchClass -> (X,Y)
  , enharmonicToXYs -- ^ (X,Y) -> [(X,Y)]
  , pcToXys         -- ^ PitchClass -> (X,Y) -> [(X,Y)]
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Monome.Types.Button
import Monome.Util


-- | Pitch is isomorphic to the integers.
-- PitchClass is isomorphic to the integers modulo 31.
-- That is, PitchClass 0 is identical to PitchClass 31,
-- whereas Pitch 31 is an octave above Pitch 0.
type Pitch      = Int
type PitchClass = Int
type LitPitches = Map PitchClass (Set LedBecause)
  -- ^ For each pitch class that is lit,
  -- we need to know why -- e.g. if it's being sustained,
  -- then we should not darken it when the finger on it is lifted,
  -- and if it's an anchor, we should never darken it.
  -- The Set is a Set because an LED could be on for multiple reasons.

et31ToFreq :: Pitch -> Float
et31ToFreq f = 2**(fi f / 31)

-- | on the PitchClass domain, xyToEt31 and et31ToLowXY are inverses:
-- xyToEt31 <$> et31ToLowXY <$> [0..31] == [0,1,2,3,4,5,6,7,8,9,10,11,12,
-- 13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0]
-- (notice the 0 at the end).
xyToEt31 :: (X,Y) -> Pitch
xyToEt31 (x,y) = 6 * x + y

-- | The numerically lowest (closest to the top-left corner)
-- member of a pitch class, if the monome is not shifted (modulo octaves).
et31ToLowXY :: PitchClass -> (X,Y)
et31ToLowXY i = (div j 6, mod j 6)
  where j = mod i 31

-- | A (maybe proper) superset of all keys that sound the same note
-- (modulo octave) visible on the monome.
enharmonicToXYs :: (X,Y) -> [(X,Y)]
enharmonicToXYs btn = map (addPair low) wideGrid
  where low = et31ToLowXY $ xyToEt31 btn
        wideGrid = [ (5*i - j, i + 6*j )
                   | i <- [0..3] , j <- [-1..2] ]

pcToXys :: (X,Y) -> PitchClass -> [(X,Y)]
pcToXys shift pc =
  enharmonicToXYs $
  addPair (et31ToLowXY pc) shift
