{-# LANGUAGE OverloadedStrings
, TupleSections #-}

module HandTest where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Control.Monad (forever)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Vivid.OSC

import Math31
import OSCMessage
import Types.Button
import Types.Device
import Util.Byte
import Util.Network


-- | = Send a message to something

testToPort port = do
  s <- toPort port
  NSB.send s $ encodeOSC $ OSC "/testing" [ OSC_S "testing" ]


-- | Test the monome.

-- toSerialosc <- sendsTo (unpack localhost) 12002
-- send toSerialosc $ requestDeviceList 8000

-- toMonome <- sendsTo (unpack localhost) 11298
-- send toMonome $ requestDeviceInfo 8000

-- send toMonome $ fade "/monome" 7 7 7
-- send toMonome $ ledOsc "/monome" ((6,6) , LedOn)

-- mapM (send toMonome . ledOsc "/monome" . (,LedOn)) $ enharmonicToXYs (0,15)
  
d = readDevice [
  OSC "/sys/id" [OSC_S "m0000102"]
  , OSC "/sys/size" [OSC_I 16,OSC_I 16]
  , OSC "/sys/host" [OSC_S "127.0.0.1"]
  , OSC "/sys/port" [OSC_I 8000]
  , OSC "/sys/prefix" [OSC_S "/monome"]
  , OSC "/sys/rotation" [OSC_I 0]
  ]

-- readSwitchOSC $ OSC "/monome/grid/key" [OSC_I 7, OSC_I 7, OSC_I 1]
