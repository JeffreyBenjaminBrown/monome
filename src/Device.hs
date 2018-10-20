{-# LANGUAGE OverloadedStrings #-}

module Device where

import Vivid.OSC

import Util.Byte


type X = Int
type Y = Int


-- | Describes how to reach a monome.
-- SerialOSC responds to /serialosc/list messages with these.
data DeviceID = DeviceID { deviceIDName :: ByteString
                         , deviceIDType :: ByteString
                         , deviceIDPort :: Int }
  deriving (Show, Eq, Ord)

readDevice :: OSC -> DeviceID
readDevice ( OSC "/serialosc/device" [ OSC_S name
                                     , OSC_S monomeType
                                     , OSC_I port ] )
  = DeviceID { deviceIDName = name
             , deviceIDType = monomeType
             , deviceIDPort = fromIntegral port }


-- | Non-contact information about a device.
-- PITFALL: a device's DeviceInfo.deviceName = its DeviceID.deviceIDName
data DeviceInfo = DeviceInfo {
  deviceName :: String       -- ^ Reported via /sys/id
  , deviceSize :: (Int, Int) -- ^ Reported via /sys/size
  , deviceHost :: String     -- ^ Where it sends to. Reported via /sys/host
  , devicePort :: Int        -- ^ Where it sends to. Reported via /sys/port
  , devicePrefix :: String   -- ^ Reported via /sys/prefix
    -- PITFALL: Includes a leading slash. I'm leaving it there.
  , deviceRotation :: Int    -- ^ Reported via /sys/rotation
  } deriving (Show, Eq, Ord)


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
                   , shineState :: Shine }
  deriving (Show, Eq, Ord)
