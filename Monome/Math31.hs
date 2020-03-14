{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, OverloadedStrings
, TupleSections #-}

module Monome.Math31 (
    et31ToFreq      -- ^ Pitch -> Float
  , xyToEt31        -- ^ (X,Y) -> Pitch
  , xyToEt31_st
  , et31ToLowXY     -- ^ PitchClass -> (X,Y)
  , enharmonicToXYs -- ^ (X,Y) -> [(X,Y)]
  , pcToXys         -- ^ PitchClass -> (X,Y) -> [(X,Y)]
  ) where

import Monome.Types.Initial
import Monome.Util


et31ToFreq :: PitchRep EtApp -> Float
et31ToFreq f = 2**(fi f / 31)

-- | on the PitchClass domain, xyToEt31 and et31ToLowXY are inverses:
-- xyToEt31 <$> et31ToLowXY <$> [0..31] == [0,1,2,3,4,5,6,7,8,9,10,11,12,
-- 13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0]
-- (notice the 0 at the end).
xyToEt31 :: (X,Y) -> PitchRep EtApp
xyToEt31 (x,y) = 6 * x + y

xyToEt31_st :: St EtApp -> (X,Y) -> PitchRep EtApp
xyToEt31_st st xy =
  xyToEt31 $ addPair xy $ negPair $ _etXyShift $ _stApp st


-- | The numerically lowest (closest to the top-left corner)
-- member of a pitch class, if the monome is not shifted (modulo octaves).
et31ToLowXY :: PitchClassRep EtApp -> (X,Y)
et31ToLowXY i = (div j 6, mod j 6)
  where j = mod i 31

-- | A (maybe proper) superset of all keys that sound the same note
-- (modulo octave) visible on the monome.
enharmonicToXYs :: (X,Y) -> [(X,Y)]
enharmonicToXYs btn = map (addPair low) wideGrid
  where low = et31ToLowXY $ xyToEt31 btn
        wideGrid = [ (5*i - j, i + 6*j )
                   | i <- [0..3] , j <- [-1..2] ]

pcToXys :: (X,Y) -> PitchClassRep EtApp -> [(X,Y)]
pcToXys shift pc =
  enharmonicToXYs $
  addPair (et31ToLowXY pc) shift
