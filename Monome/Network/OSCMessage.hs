{-# LANGUAGE OverloadedStrings #-}

module Monome.Network.OSCMessage where

import Vivid.OSC

import Monome.Util
import Monome.Network.Util (localhost)


-- | PITFALL: The monome will respond to out-of-bounds (x,y) values.
-- Therefore every Window should have a nontrivial windowContains field,
-- even if it is the background Window.


-- | to the SerialOSC server

requestDeviceList :: Int -> ByteString
requestDeviceList mailboxPort = do
  encodeOSC $ OSC "/serialosc/list" [ OSC_S localhost
                                    , OSC_I $ fi mailboxPort ]


-- | to a device, esp. a monome

requestDeviceInfo :: Int -> ByteString
requestDeviceInfo mailboxPort = do
  encodeOSC $ OSC "/sys/info" [ OSC_S localhost, OSC_I $ fi mailboxPort ]

requestSendTo :: Int -> [ByteString]
requestSendTo mailboxPort =
  [ encodeOSC $ OSC "/sys/host" [OSC_S localhost]
  , encodeOSC $ OSC "/sys/port" [OSC_I $ fi mailboxPort]
  ]

fade :: String -> Int -> Int -> Int -> ByteString
fade devicePrefix x y l = do -- ^ fade light level from 0 to 15
  encodeOSC $ OSC (pack $ devicePrefix ++ "/grid/led/level/set")
    [ OSC_I $ fi x, OSC_I $ fi y, OSC_I $ fi l ]

onoff :: String -> Int -> Int -> Int -> ByteString
onoff devicePrefix x y l = do -- ^ toggle light level, 0 or 1
  encodeOSC $ OSC (pack $ devicePrefix ++ "/grid/led/set")
    [ OSC_I $ fi x, OSC_I $ fi y, OSC_I $ fi l ]

allLeds :: String -> Int -> ByteString
allLeds devicePrefix l = do -- ^ toggle light level, 0 or 1
  encodeOSC $ OSC (pack $ devicePrefix ++ "/grid/led/all")
    [ OSC_I $ fi l ]
