-- This is straight from the README file for Vivid.OSC:
-- https://hackage.haskell.org/package/vivid-osc
-- except I tweaked the port to 12002 and the message to be something
-- that a monome can understand.

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
