{-# LANGUAGE OverloadedStrings #-}

module Monome.Types.Button (
  X, Y
  , Switch(..), Led(..), LedBecause(..)
  , readSwitchOSC, switchToInt, switchFromInt
  , ledToInt, ledFromInt
  , ledOsc, allLedOsc
  ) where

import Vivid.OSC

import Monome.Util
import Monome.Network.Monome


-- | PITFALL: X rises from left to right, but Y rises from top to bottom.
-- Thus (0,1) is just under the top-left corner.
type X = Int
type Y = Int


-- | The mechanical state of a monome button
data Switch = SwitchOn | SwitchOff
  deriving (Show, Eq, Ord)

switchToInt :: Switch -> Int
switchToInt SwitchOn = 1
switchToInt SwitchOff = 0

switchFromInt :: Int -> Switch
switchFromInt 0 = SwitchOff
switchFromInt 1 = SwitchOn
switchFromInt x = error $ "switchFromInt: " ++ show x
                  ++ " is niether 0 nor 1."

readSwitchOSC :: OSC -> ((X,Y), Switch)
readSwitchOSC (OSC "/monome/grid/key" [OSC_I x, OSC_I y, OSC_I s]) =
  ((fi x, fi y), switchFromInt $ fi s)
readSwitchOSC x = error $ "readSwitchOSC: bad message: " ++ show x

-- | The state of a monome LED
data Led = LedOn | LedOff
  deriving (Show, Eq, Ord)

ledToInt :: Led -> Int
ledToInt LedOff = 0
ledToInt LedOn = 1

ledFromInt :: Int -> Led
ledFromInt 0 = LedOff
ledFromInt 1 = LedOn
ledFromInt x = error $ "ledFromInt: " ++ show x ++ " is neither 0 nor 1."


-- | Tells the monome to turn on an LED. See Test/HandTest.hs.
ledOsc :: String -> ((X,Y), Led) -> ByteString
ledOsc prefix ((x, y), led) = onoff prefix x y $ ledToInt led

-- | Tells the monome to turn on every LED. See Test/HandTest.hs.
allLedOsc :: String -> Led -> ByteString
allLedOsc prefix led = allLeds prefix $ ledToInt led


-- | The reason an Led is lit.
data LedBecause =
    LedBecauseSwitch  (X,Y)
  | LedBecauseSustain (X,Y)
  | LedBecauseAnchor -- ^ Some "visual anchor" pitches are always on.
  deriving (Show, Eq, Ord)
