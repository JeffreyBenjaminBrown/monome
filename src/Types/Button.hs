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
data Light = Lit | Dark
  deriving (Show, Eq, Ord)

lightToInt :: Light -> Int
lightToInt Lit = 1
lightToInt Dark = 0

lightFromInt :: Int -> Light
lightFromInt 0 = Dark
lightFromInt 1 = Lit


-- | The act of changing a monome LED's Light
data Shine = Shine { shineX :: X
                   , shineY :: Y
                   , shineLight :: Light }
  deriving (Show, Eq, Ord)

shineToOscByte :: String -> Shine -> ByteString
shineToOscByte prefix s = onoff prefix (shineX s) (shineY s)
  (lightToInt $ shineLight s)
