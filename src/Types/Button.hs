{-# LANGUAGE OverloadedStrings #-}

module Types.Button where

import Vivid.OSC

import Util.Byte
import Util.Network (HostName)
import OSCMessage


type X = Int
type Y = Int


-- | The mechanical state of a monome button
data Pressure = Pressed | Unpressed
  deriving (Show, Eq, Ord)

pressureToInt :: Pressure -> Int
pressureToInt Pressed = 1
pressureToInt Unpressed = 0

pressureFromInt :: Int -> Pressure
pressureFromInt 0 = Unpressed
pressureFromInt 1 = Pressed


-- | The act of changing a monome button's Pressure
data Press = Press { pressX :: X
                   , pressY :: Y
                   , pressState :: Pressure }
  deriving (Show, Eq, Ord)

readPress :: OSC -> Press
readPress (OSC "/monome/grid/key" [OSC_I x, OSC_I y, OSC_I s]) =
  Press { pressX     =                   fi x
        , pressY     =                   fi y
        , pressState = pressureFromInt $ fi s}


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
