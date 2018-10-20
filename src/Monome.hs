-- | This communicates with serialosc. Run the mailbox in one REPL
-- and then call `requestMonomeInfo` from another.
--
-- serialosc protocol: https://monome.org/docs/osc/

{-# LANGUAGE OverloadedStrings #-}

module Monome where

import qualified Control.Concurrent as CC
import Control.Concurrent.MVar
import Control.Monad (forever)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import Vivid.OSC

import Util


data Device = Device { deviceName :: BS.ByteString
                     , deviceType :: BS.ByteString
                     , devicePort :: Int } deriving (Show, Eq, Ord)

readDevice :: OSC -> Device
readDevice ( OSC "/serialosc/device" [ OSC_S name
                                     , OSC_S monomeType
                                     , OSC_I port ] )
  = Device { deviceName = name
           , deviceType = monomeType
           , devicePort = fromIntegral port }

nonsenseToPort port = do
  s <- sendsTo "127.0.0.1" 11111
  NSB.send s $ encodeOSC $ OSC "/testing/testing" [
    OSC_S "quack oink wackadoo"
    , OSC_S "127.0.0.1"
    , OSC_I port
    ]

requestDeviceList = do
  s <- sendsTo "127.0.0.1" 12002
  NSB.send s $ encodeOSC $ OSC "/serialosc/list" [
    OSC_S "127.0.0.1"
    , OSC_I 11111
    ]

requestDeviceInfo devicePort = do
  s <- sendsTo "127.0.0.1" devicePort
  NSB.send s $ encodeOSC $ OSC "/sys/info" [
    OSC_S "127.0.0.1"
    , OSC_I 11111
    ]

requestSendTo devicePort mailbox = do
  s <- sendsTo "127.0.0.1" devicePort
  NSB.send s $ encodeOSC $ OSC "/sys/port" [OSC_I mailbox]
  -- NSB.send s $ encodeOSC $ OSC "/sys/host" [OSC_S "127.0.0.1"]

fade prefix devicePort x y l = do -- ^ fade light level from 0 to 15
  s <- sendsTo "127.0.0.1" devicePort
  NSB.send s $ encodeOSC $ OSC (pack $ prefix ++ "/grid/led/level/set")
    [ OSC_I x, OSC_I y, OSC_I l ]

onoff prefix devicePort x y l = do -- ^ toggle light level, 0 or 1
  s <- sendsTo "127.0.0.1" devicePort
  NSB.send s $ encodeOSC $ OSC (pack $ prefix ++ "/grid/led/set")
    [ OSC_I x, OSC_I y, OSC_I l ]

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
  mailbox <- CC.forkIO $
    forever $ decodeOSC <$> NSB.recv s 4096 >>= printAndShowEitherOsc
  loop
