import Util.ByteString
import Util.Network


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
