-- This is straight from the README file for Vivid.OSC:
-- https://hackage.haskell.org/package/vivid-osc
-- except I tweaked the port to 12002 and the message to be something
-- that a monome can understand.

{-# LANGUAGE OverloadedStrings #-}

module Demo where

import Control.Concurrent (threadDelay)
import Network.Socket
import Network.Socket.ByteString as SB
import Vivid.OSC


main = do
   -- setup
   (a:_) <- getAddrInfo Nothing (Just "127.0.0.1") (Just "12002")
   s <- socket (addrFamily a) Datagram defaultProtocol
   connect s $ addrAddress a

   -- send a message
   SB.send s $ encodeOSC $ OSC "/monome/grid/led/set" [OSC_I 1, OSC_I 1, OSC_I 15]

