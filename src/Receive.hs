module Receive where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.ByteString.Char8 (pack)
import Network.Socket (close)
import Network.Socket.ByteString (recv)
import Vivid.OSC

import Util


mailbox :: IO [OSC]
mailbox = do
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
