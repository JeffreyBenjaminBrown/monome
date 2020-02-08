{-# LANGUAGE
LambdaCase
, OverloadedStrings #-}


module Monome.Mailbox where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Vivid.OSC

import Monome.Util.Byte
import Monome.Util.Network


-- ^ Tries to read as OSC, then prints (as OSC or otherwise).
-- Also accumulates a list of OSC messages.
-- Useful when running `requestDeviceList` or `requestDeviceInfo`
-- from another repl.
mailbox :: Int -> IO [OSC]
mailbox port = do
  s <- receivesAt "127.0.0.1" port
  acc <- newMVar []
  let loop :: IO [OSC]
      loop = getChar >>= \case 'q' -> close s >> readMVar acc >>= return
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
