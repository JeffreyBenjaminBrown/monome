{-# LANGUAGE OverloadedStrings #-}

module Device where

import Vivid.OSC

import Util.Byte
import Util.Network (HostName)
import OSCMessage


type X = Int
type Y = Int


-- | SerialOsc responds to /serialosc/list messages with this information.
data DeviceID = DeviceID { deviceIDName :: ByteString
                         , deviceIDType :: ByteString
                         , deviceIDPort :: Int }
  deriving (Show, Eq, Ord)

readDeviceID :: OSC -> DeviceID
readDeviceID ( OSC "/serialosc/device" [ OSC_S name
                                       , OSC_S monomeType
                                       , OSC_I port ] )
  = DeviceID { deviceIDName = name
             , deviceIDType = monomeType
             , deviceIDPort = fromIntegral port }


-- | A monome (distinct form serialosc!) responds to /sys/info messages
-- with this information.
--
-- A device's DeviceInfo.deviceName = its DeviceID.deviceIDName.
-- That is the only pooint where `Device` and `DeviceID` overlap.
data Device = Device {
  deviceName :: String
  , deviceSize :: (X,Y)
  , deviceHost :: HostName   -- ^ Where it sends to.
  , devicePort :: Int        -- ^ Where it sends to.
  , devicePrefix :: String   -- ^ PITFALL: Includes a leading slash.
  , deviceRotation :: Int    -- ^ 0, 90, 180 or 270
  } deriving (Show, Eq, Ord)

readDeviceName :: OSC -> String
readDeviceName (OSC "/sys/id" [OSC_S name]) = unpack name
readDeviceSize :: OSC -> (X,Y)
readDeviceSize (OSC "/sys/size" [OSC_I x, OSC_I y]) = (fi x, fi y)
readDeviceHost :: OSC -> HostName
readDeviceHost (OSC "/sys/host" [OSC_S name]) = unpack name
readDevicePort :: OSC -> Int
readDevicePort (OSC "/sys/port" [OSC_I port]) = fi port
readDevicePrefix :: OSC -> String
readDevicePrefix (OSC "/sys/prefix" [OSC_S prefix]) = unpack prefix
readDeviceRotation :: OSC -> Int
readDeviceRotation (OSC "/sys/rotation" [OSC_I rotation]) = fi rotation

-- | PITFALL: If serialosc changed the order of its outputs, this would fail.
readDevice :: [OSC] -> Device
readDevice [a,b,c,d,e,f] = Device {
    deviceName = readDeviceName a
  , deviceSize = readDeviceSize b
  , deviceHost = readDeviceHost c
  , devicePort = readDevicePort d
  , devicePrefix = readDevicePrefix e
  , deviceRotation = readDeviceRotation f
  }


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
