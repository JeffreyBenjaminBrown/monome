-- | This is almost identical to Demo.hs, which works.
-- But this attempts (unsuccessfully) to communicate with serialosc.
--
-- According to the docs[1], serialosc listens on port 12002,
-- and when it receives a message of this form:
--    /sys/info host port
-- (for instance /sys/info 127.0.0.1 0), it will send
-- information about connected devices to that port.
--
-- [1] https://monome.org/docs/osc/

{-# LANGUAGE OverloadedStrings #-}

module Monome where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Network.Socket
import Network.Socket.ByteString as SB
import Vivid.OSC

import Util


requestMonomeInfo = do
  s <- sendsTo 12002
  SB.send s $ encodeOSC $ OSC "/sys/info" [
    OSC_S "127.0.0.1", -- this is localhost, so it should be omittable
    OSC_I 0
    ]

monomeMailbox = do
  s <- receivesAt 0
  forever $ decodeOSC <$> SB.recv s 4096 >>= putStrLn . show
