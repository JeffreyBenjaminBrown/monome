{-# LANGUAGE DataKinds, ExtendedDefaultRules, OverloadedStrings #-}

module Mailbox where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Vivid
import Vivid.OSC

import Util.Byte
import Util.Network
import Device


theSound = sd (0 ::I "note") $ do
   wobble <- sinOsc (freq_ 5) ? KR ~* 10 ~+ 10
   s <- 0.1 ~* sinOsc (freq_ $ midiCPS (V::V "note") ~+ wobble)
   out 0 [s,s]

playKey :: OSC -> IO ()
playKey osc@(OSC "/monome/grid/key" _) =
  let (Press x y s) = readPress osc
  in do s0 <- synth theSound (36 ::I "note")
        wait 1
        free s0
playKey osc = putStrLn . show $ osc

mailboxSynths :: IO [OSC]
mailboxSynths = do
  s <- receivesAt "127.0.0.1" 11111
  acc <- newMVar []
  let loop :: IO [OSC]
      loop = do cmd <- getChar
                case cmd of 'q' -> close s >> readMVar acc >>= return
                            _   -> loop
  mailbox <- forkIO $ forever $
    decodeOSC <$> recv s 4096 >>= either (putStrLn . show) playKey
  loop

-- ^ Tries to read as OSC, then prints (as OSC or otherwise).
-- Useful when running `requestDeviceList` from another repl.
mailboxPrints :: IO [OSC]
mailboxPrints = do
  s <- receivesAt "127.0.0.1" 11111
  acc <- newMVar []
  let loop :: IO [OSC]
      loop = do cmd <- getChar
                case cmd of 'q' -> close s >> readMVar acc >>= return
                            _   -> loop
      printAndShow :: OSC -> IO ()
      printAndShow osc = do accNow <- takeMVar acc
                            putMVar acc $ osc : accNow
                            putStrLn . show $ osc
      printAndShowEitherOsc :: Either String OSC -> IO ()
      printAndShowEitherOsc (Left s) = putStrLn . show $ s
      printAndShowEitherOsc (Right osc) = printAndShow osc
  mailbox <- forkIO $
    forever $ decodeOSC <$> recv s 4096 >>= printAndShowEitherOsc
  loop
