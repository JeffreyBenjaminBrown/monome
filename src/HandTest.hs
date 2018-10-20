import Util.Byte
import Util.Network

:set -XOverloadedStrings

-- | = Send a message to something

testToPort port = do
  s <- toPort port
  NSB.send s $ encodeOSC $ OSC "/testing" [ OSC_S "testing" ]


-- | Test the monome

toSerialosc <- sendsTo (unpack localhost) 12002
send toSerialosc $ requestDeviceList 11111

toMonome <- sendsTo (unpack localhost) 13993
send toMonome $ requestDeviceInfo 11111

send toMonome $ fade "/monome" 7 7 7

:{
d = readDevice [
    OSC "/sys/id" [OSC_S "m0000102"]
  , OSC "/sys/size" [OSC_I 16,OSC_I 16]
  , OSC "/sys/host" [OSC_S "127.0.0.1"]
  , OSC "/sys/port" [OSC_I 11111]
  , OSC "/sys/prefix" [OSC_S "/monome"]
  , OSC "/sys/rotation" [OSC_I 0]
  ]
:}
