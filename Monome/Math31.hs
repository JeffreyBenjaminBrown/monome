{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, OverloadedStrings
, TupleSections #-}

module Monome.Math31 (
    edo, spacing    -- ^ Num a => a
  , et31ToFreq      -- ^ Pitch -> Float
  , xyToEt31        -- ^ (X,Y) -> Pitch
  , xyToEt31_st
  , et31ToLowXY     -- ^ PitchClass -> (X,Y)
  , enharmonicToXYs -- ^ (X,Y) -> [(X,Y)]
  , pcToXys         -- ^ PitchClass -> (X,Y) -> [(X,Y)]
  ) where

import Monome.Types.Initial
import Monome.Util

edo, spacing :: Num a => a
edo = 41
spacing = 7 -- number of edo steps between one row and the next

et31ToFreq :: Pitch EdoApp -> Float
et31ToFreq f = 2**(fi f / edo)

-- | on the PitchClass domain, xyToEt31 and et31ToLowXY are inverses:
-- xyToEt31 <$> et31ToLowXY <$> [0..31] == [0,1,2,3,4,5,6,7,8,9,10,11,12,
-- 13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0]
-- (notice the 0 at the end).
xyToEt31 :: (X,Y) -> Pitch EdoApp
xyToEt31 (x,y) = spacing * x + y

xyToEt31_st :: St EdoApp -> (X,Y) -> Pitch EdoApp
xyToEt31_st st xy =
  xyToEt31 $ addPair xy $ negPair $ _etXyShift $ _stApp st


-- | The numerically lowest (closest to the top-left corner)
-- member of a pitch class, if the monome is not shifted (modulo octaves).
et31ToLowXY :: PitchClass EdoApp -> (X,Y)
et31ToLowXY i = (div j spacing, mod j spacing)
  where j = mod i edo

-- | A (maybe proper) superset of all keys that sound the same note
-- (modulo octave) visible on the monome.
enharmonicToXYs :: (X,Y) -> [(X,Y)]
enharmonicToXYs btn = map (addPair low) wideGrid
  -- The 31et grid generators are (5,1) and (-1,6),
  -- which implies the grid formula (5i-j, i+6j).
  -- Generators used here: (6,-1) and (-1,7).
  where low = et31ToLowXY $ xyToEt31 btn
        wideGrid = [ (6*i - j, -i + 7*j)
                   | i <- [0..2] , j <- [0..2] ]

pcToXys :: (X,Y) -> PitchClass EdoApp -> [(X,Y)]
pcToXys shift pc =
  enharmonicToXYs $
  addPair (et31ToLowXY pc) shift
