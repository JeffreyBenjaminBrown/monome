{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, OverloadedStrings
, TupleSections #-}

module ET31.Keyboard (
  xyToEt31
  , et31ToFreq
  , enharmonicKeys
  ) where

import Vivid

import Synth
import Types.Button
import Util.Byte


xyToEt31 :: (X,Y) -> Float
xyToEt31 (x,y) = fi (15-x) + 6 * fi y

et31ToFreq :: Float -> Float
et31ToFreq f = 2**(f/31)

enharmonicKeys :: (X,Y) -> [(X,Y)]
enharmonicKeys (x,y) =
  filter (\(x,y) -> numBetween x 0 15 && numBetween y 0 15)
  $ [(x + x' * 6 + y' * (-1), y + x' * 1 + y' * 5 )
    | x' <- [-2..2], y' <- [-3..3] ]
