{-# LANGUAGE OverloadedStrings #-}

module Monome.Types.Button (
  X, Y, Switch, Led, LedBecause(..)
  , readSwitchOSC, boolToInt, boolFromInt
  , ledOsc, allLedOsc
  ) where

import Vivid.OSC

import Monome.Util
import Monome.Network.Monome


-- | PITFALL: X rises from left to right, but Y rises from top to bottom.
-- Thus (0,1) is just under the top-left corner.
type X = Int
type Y = Int

type Switch = Bool -- | Whether a monome button is pressed.
type Led    = Bool -- | Whether a monome LED is lit.

-- | The reason a (pitch class of) LED(s) in the keyboard window is lit.
data LedBecause =
    LedBecauseSwitch (X,Y)
  | LedBecauseSustain
  | LedBecauseAnchor -- ^ Some "visual anchor" pitches are always on.
  deriving (Show, Eq, Ord)

boolToInt :: Num a => Bool -> a
boolToInt True = 1
boolToInt False = 0

boolFromInt :: Int -> Bool
boolFromInt 0 = False
boolFromInt 1 = True
boolFromInt x = error $ "boolFromInt: " ++ show x
                  ++ " is niether 0 nor 1."

readSwitchOSC :: OSC -> ((X,Y), Switch)
readSwitchOSC (OSC "/monome/grid/key" [OSC_I x, OSC_I y, OSC_I s]) =
  ((fi x, fi y), boolFromInt $ fi s)
readSwitchOSC x = error $ "readSwitchOSC: bad message: " ++ show x

-- | Tells the monome to turn on an LED. See Test/HandTest.hs.
ledOsc :: String -> ((X,Y), Led) -> ByteString
ledOsc prefix ((x, y), led) = onoff prefix x y $ boolToInt led

-- | Tells the monome to turn on every LED. See Test/HandTest.hs.
allLedOsc :: String -> Led -> ByteString
allLedOsc prefix led = allLeds prefix $ boolToInt led
