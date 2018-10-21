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


type BoopParams = '["freq","amp"]

boop :: SynthDef BoopParams
boop = sd ( 0 :: I "freq"
          , 0 :: I "amp"
          ) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
   out 0 [s1, s1]

playKey :: Synth BoopParams -> OSC -> IO ()
playKey sy osc@(OSC "/monome/grid/key" _) = do
  putStrLn $ show osc
  let (Press x y p) = readPress osc
  set sy $ (toI $ 0.02 * fi (pressureToInt p) :: I "amp")
  set sy $ (toI $ 100 + x + 10*y :: I "freq")
playKey _ osc = putStrLn . show $ osc

mailboxSynths :: IO [OSC]
mailboxSynths = do
  s <- receivesAt "127.0.0.1" 11111
  acc <- newMVar []
--  spots = [(a,b) | a <- [1..16], b <- [1..16]]
--  voices <- zip spots <$> mapM (synth boop) (repeat $ 0 :: I "amp")
  voice <- synth boop ()
  let loop :: IO [OSC]
      loop = do cmd <- getChar
                case cmd of 'q' -> close s >> readMVar acc >>= return
                            _   -> loop
  mailbox <- forkIO $ forever $
    decodeOSC <$> recv s 4096 >>= either (putStrLn . show) (playKey voice)
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
