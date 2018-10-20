{-# LANGUAGE OverloadedStrings #-}

module OSCMessage where

import Vivid.OSC

import Util.ByteString
import Util.Network (localhost)


-- | to the SerialOSC server

requestDeviceList mailboxPort = do
  encodeOSC $ OSC "/serialosc/list" [ OSC_S localhost
                                    , OSC_I mailboxPort ]


-- | to a device, esp. a monome

requestDeviceInfo mailboxPort = do
  encodeOSC $ OSC "/sys/info" [ OSC_S localhost, OSC_I mailboxPort ]

requestSendTo mailboxPort =
  [ encodeOSC $ OSC "/sys/host" [OSC_S localhost]
  , encodeOSC $ OSC "/sys/port" [OSC_I mailboxPort]
  ]

fade devicePrefix x y l = do -- ^ fade light level from 0 to 15
  encodeOSC $ OSC (pack $ devicePrefix ++ "/grid/led/level/set")
    [ OSC_I x, OSC_I y, OSC_I l ]

onoff devicePrefix x y l = do -- ^ toggle light level, 0 or 1
  encodeOSC $ OSC (pack $ devicePrefix ++ "/grid/led/set")
    [ OSC_I x, OSC_I y, OSC_I l ]
