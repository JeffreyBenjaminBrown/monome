{-# LANGUAGE OverloadedStrings #-}

module Send where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Network.Socket.ByteString (send)
import Vivid.OSC
import Util


localhost = "127.0.0.1" :: ByteString
toPort port = sendsTo (unpack localhost) port
toSerialosc = toPort 12002

testToPort port = do
  s <- toPort port
  send s $ encodeOSC $ OSC "/testing" [ OSC_S "testing" ]


-- | to the SerialOSC server

requestDeviceList mailboxPort = do
  s <- toSerialosc
  send s $ encodeOSC $ OSC "/serialosc/list" [ OSC_S localhost
                                             , OSC_I mailboxPort ]


-- | to a device, esp. a monome

requestDeviceInfo mailboxPort devicePort = do
  s <- toPort devicePort
  send s $ encodeOSC $ OSC "/sys/info" [ OSC_S localhost, OSC_I mailboxPort ]

requestSendTo mailboxPort devicePort = do
  s <- toPort devicePort
  send s $ encodeOSC $ OSC "/sys/host" [OSC_S localhost]
  send s $ encodeOSC $ OSC "/sys/port" [OSC_I mailboxPort]

fade devicePort devicePrefix x y l = do -- ^ fade light level from 0 to 15
  s <- toPort devicePort
  send s $ encodeOSC $ OSC (pack $ devicePrefix ++ "/grid/led/level/set")
    [ OSC_I x, OSC_I y, OSC_I l ]

onoff devicePort devicePrefix x y l = do -- ^ toggle light level, 0 or 1
  s <- toPort devicePort
  send s $ encodeOSC $ OSC (pack $ devicePrefix ++ "/grid/led/set")
    [ OSC_I x, OSC_I y, OSC_I l ]
