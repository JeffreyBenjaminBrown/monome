{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, OverloadedStrings
, TupleSections #-}

module Monome.Math31 (
  Pitch, PitchClass
  , et31ToFreq
  , xyToEt31
  , et31ToLowXY
  , enharmonicToXYs
  ) where

import Vivid

import Monome.Synth
import Monome.Types.Button
import Monome.Util.Byte


-- | PitchClass and Pitch differ in that PitchClass 0 should be
-- inndistinguishable from PitchClass 31, whereas Pitch 31 is
-- an octave higher than Pitch 0.
type Pitch = Int
type PitchClass = Int


et31ToFreq :: Pitch -> Float
et31ToFreq f = 2**(fi f / 31)

-- | on the PitchClass domain, xyToEt31 and et31ToLowXY are inverses:
-- xyToEt31 <$> et31ToLowXY <$> [0..31] == [0,1,2,3,4,5,6,7,8,9,10,11,12,
-- 13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0]
-- (notice the 0 at the end).
xyToEt31 :: (X,Y) -> Pitch
xyToEt31 (x,y) = 6 * x + y

et31ToLowXY :: PitchClass -> (X,Y)
et31ToLowXY i = (div j 6, mod j 6)
  where j = mod i 31

enharmonicToXYs :: (X,Y) -> [(X,Y)]
enharmonicToXYs xy = map (addPair low) wideGrid
  where low = et31ToLowXY $ xyToEt31 xy
        wideGrid = [ (5*i - j, i + 6*j )
                   | i <- [0..3] , j <- [-1..2] ]
