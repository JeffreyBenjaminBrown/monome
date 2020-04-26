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
edo = 31  -- Pick your temperament.
spacing = 6 -- Pick the number of edo steps between one row
            -- and the next. Negative doesn't work yet.
  -- Some combinations I like: (31,6), (41,6), (46,7)

vv = (-1,spacing)
hv = let x = head $ filter (> edo)
             $ (*spacing) <$> [1..]
     in (div x spacing, edo - x)

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
  where low = et31ToLowXY $ xyToEt31 btn
        ((v1,v2),(h1,h2)) = (vv,hv)
        wideGrid = [ ( i*h1 + j*v1
                     , i*h2 + j*v2 )
                   | i <- [0..3] , j <- [0..3] ]

pcToXys :: (X,Y) -> PitchClass EdoApp -> [(X,Y)]
pcToXys shift pc =
  enharmonicToXYs $
  addPair (et31ToLowXY pc) shift
