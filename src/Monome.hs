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
  s <- sendsTo "127.0.0.1" 12002
  SB.send s $ encodeOSC $ OSC "/sys/info" [
    OSC_S "127.0.0.1"
    , OSC_I 11111
    ]

testMonomeMailbox = do
  s <- sendsTo "127.0.0.1" 11111
  SB.send s $ encodeOSC $ OSC "/testing/the/mailbox" [
    OSC_S "Here, mailbox, have a string. And this number too:"
    , OSC_I 0
    ]

monomeMailbox = do
  s <- receivesAt "127.0.0.1" 11111
  forever $ decodeOSC <$> SB.recv s 4096 >>= putStrLn . show
