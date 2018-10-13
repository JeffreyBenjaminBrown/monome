{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Monad (forever)
import Network.Socket
import Network.Socket.ByteString as SB
import Vivid.OSC


getLocalSocket i = do
  (a:_) <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ show i)
  s <- socket (addrFamily a) Datagram defaultProtocol
  return (s,a)

sendsTo i = do
  (s,a) <- getLocalSocket i
  connect s $ addrAddress a
  return s

receivesAt i = do
  (s,a) <- getLocalSocket i
  bind s $ addrAddress a
  return s
