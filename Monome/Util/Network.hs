{-# LANGUAGE OverloadedStrings #-}

module Monome.Util.Network where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Control.Monad (forever)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Vivid.OSC


-- | = PITFALL: Ports 0-1024 are reserved.
-- All 5-digit numbers seem to work, though.

close = NS.close
recv = NSB.recv
send = NSB.send
type HostName = NS.HostName
type Socket = NS.Socket

localhost :: ByteString
localhost = "127.0.0.1"

toPort :: Show a => a -> IO NS.Socket
toPort port = sendsTo (unpack localhost) port
toSerialosc = toPort 12002

getLocalSocket :: Show a
               => NS.HostName -> a -> IO (NS.Socket, NS.AddrInfo)
getLocalSocket host port = do
  (a:_) <- NS.getAddrInfo Nothing (Just host) (Just $ show port)
  s <- NS.socket (NS.addrFamily a) NS.Datagram NS.defaultProtocol
  return (s,a)

sendsTo :: Show a => NS.HostName -> a -> IO NS.Socket
sendsTo host port = do
  (s,a) <- getLocalSocket host port
  NS.connect s $ NS.addrAddress a
  return s

receivesAt :: Show a => NS.HostName -> a -> IO NS.Socket
receivesAt host port = do
  (s,a) <- getLocalSocket host port
  NS.bind s $ NS.addrAddress a
  return s
