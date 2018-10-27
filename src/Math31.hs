{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, OverloadedStrings
, TupleSections #-}

module Math31 (
  xyToEt31
  , et31ToFreq
  , et31ToLowXY
  , enharmonicToXYs
  ) where

import Vivid

import Synth
import Types.Button
import Util.Byte


-- | xyToEt31 and et31ToLowXY are roughly inverses:
-- xyToEt31 <$> et31ToLowXY <$> [0..31] == [0,1,2,3,4,5,6,7,8,9,10,11,12,
-- 13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0]
-- (notice the 0 at the end).
xyToEt31 :: (X,Y) -> Float
xyToEt31 (x,y) = fi (15-x) + 6 * fi y

et31ToLowXY :: Int -> (X,Y)
et31ToLowXY i = let i' = mod i 31
  in (15 - mod i' 6, div i' 6)

et31ToFreq :: Float -> Float
et31ToFreq f = 2**(f/31)

enharmonicToXYs :: (X,Y) -> [(X,Y)]
enharmonicToXYs (x,y) =
  filter (\(x,y) -> numBetween x 0 15 && numBetween y 0 15)
  $ [(x + x' * 6 + y' * (-1), y + x' * 1 + y' * 5 )
    | x' <- [-2..2], y' <- [-3..3] ]
