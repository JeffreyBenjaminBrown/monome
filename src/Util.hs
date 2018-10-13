{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Monad (forever)
import Network.Socket
import Network.Socket.ByteString as SB
import Vivid.OSC


-- | = PITFALL: Port 0 fails, but every 5-digit numbers I've tried works.

getLocalSocket host port = do
  (a:_) <- getAddrInfo Nothing (Just host) (Just $ show port)
  s <- socket (addrFamily a) Datagram defaultProtocol
  return (s,a)

sendsTo host port = do
  (s,a) <- getLocalSocket host port
  connect s $ addrAddress a
  return s

receivesAt host port = do
  (s,a) <- getLocalSocket host port
  bind s $ addrAddress a
  return s
