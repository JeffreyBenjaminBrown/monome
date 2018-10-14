-- | This communicates with serialosc. Run the mailbox in one REPL
-- and then call `requestMonomeInfo` from another.
--
-- serialosc protocol: https://monome.org/docs/osc/

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
  SB.send s $ encodeOSC $ OSC "/serialosc/list" [
    OSC_S "127.0.0.1"
    , OSC_I 11111
    ]

monomeMailbox = do
  s <- receivesAt "127.0.0.1" 11111
  forever $ decodeOSC <$> SB.recv s 4096 >>= putStrLn . show
