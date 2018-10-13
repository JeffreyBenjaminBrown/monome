-- | Demonstrates how to send and receive with Vivid.OSC.
--
-- If you run two separate instances of GHCI, you can start one of the
-- `mailbox*` functions below in one of them,
-- call one of the `send*` functions below from the other,
-- and see the first respond to the second.
--
-- Basically pulled straight from the README file for Vivid.OSC:
-- https://hackage.haskell.org/package/vivid-osc


{-# LANGUAGE OverloadedStrings #-}

module Demo where

import Control.Monad (forever)
import Network.Socket
import Network.Socket.ByteString as SB
import Vivid.OSC

import Util


sendTest = do
  s <- sendsTo 57120
  SB.send s $ encodeOSC $ OSC "/Every/OSC/message/starts like/this" [
      OSC_S "This one includes this string and a nice round number."
    , OSC_F pi
    ]

sendBoom = do -- ^ Messages in TidalCycles look like this.
  s <- sendsTo 57120
  SB.send s $ encodeOSC $
    OSC "/play2" [OSC_S "cps", OSC_F 1.2, OSC_S "s", OSC_S "bd"]

mailboxPrints = do
  s <- receivesAt 57120
  forever $ decodeOSC <$> SB.recv s 4096 >>= putStrLn . show

mailboxInterprets = do
  s <- receivesAt 57120
  forever $ do
    o <- decodeOSC <$> SB.recv s 4096
    case o of
      Right (OSC "/play2" [_, OSC_F vel, _, OSC_S "bd"]) ->
        putStrLn $ if vel < 1
          then "boom"
          else "BOOM!"
      _ -> putStrLn $ "Unexpected input: "++show o
