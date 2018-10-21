{-# LANGUAGE OverloadedStrings #-}

module Mailbox.PrinterKeeper where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Vivid.OSC

import Util.Byte
import Util.Network


-- ^ Tries to read as OSC, then prints (as OSC or otherwise).
-- Also accumulates a list of OSC messages.
-- Useful when running `requestDeviceList` from another repl.
printerKeeper :: IO [OSC]
printerKeeper = do
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
