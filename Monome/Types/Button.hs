{-# LANGUAGE OverloadedStrings #-}

module Monome.Types.Button (
  X, Y
  , Switch(..), Led(..), LedReason(..)
  , readSwitchOSC, switchToInt, switchFromInt
  , ledToInt, ledFromInt
  , ledOsc, allLedOsc
  ) where

import Vivid.OSC

import Monome.Util.Byte
import Monome.Util.Network (HostName)
import Monome.OSCMessage


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

readSwitchOSC :: OSC -> ((X,Y), Switch)
readSwitchOSC (OSC "/monome/grid/key" [OSC_I x, OSC_I y, OSC_I s]) =
  ((fi x, fi y), switchFromInt $ fi s)


-- | The state of a monome LED
data Led = LedOn | LedOff
  deriving (Show, Eq, Ord)

ledToInt :: Led -> Int
ledToInt LedOff = 0
ledToInt LedOn = 1

ledFromInt :: Int -> Led
ledFromInt 0 = LedOff
ledFromInt 1 = LedOn


-- | The act of changing a monome LED's Light
ledOsc :: String -> ((X,Y), Led) -> ByteString
ledOsc prefix ((x, y), led) = onoff prefix x y $ ledToInt led

allLedOsc :: String -> Led -> ByteString
allLedOsc prefix led = allLeds prefix $ ledToInt led


-- | The reason an Led is lit.
data LedReason =
    LedFromSwitch  {xy :: (X,Y)}
  | LedFromSustain {xy :: (X,Y)}
  | LedFromAnchor -- ^ Some "visual anchor" pitches are always on.
  deriving (Show, Eq, Ord)
