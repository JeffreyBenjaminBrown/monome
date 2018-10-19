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

nonsense = do
  s <- sendsTo "127.0.0.1" 11111
  NSB.send s $ encodeOSC $ OSC "/quack/bark" [OSC_S "oof"
                                             , OSC_S "127.0.0.1"
                                             , OSC_I 11111
                                             ]

requestDeviceList = do
  s <- sendsTo "127.0.0.1" 12002
  NSB.send s $ encodeOSC $ OSC "/serialosc/list" [
    OSC_S "127.0.0.1"
    , OSC_I 11111
    ]

requestInfo devicePort = do
  -- ^ TODO : serialosc appears not to respond.
  -- But this is just like requestDeviceList, to which it does.
  s <- sendsTo "127.0.0.1" devicePort
  NSB.send s $ encodeOSC $ OSC "/sys/info" [
    OSC_S "127.0.0.1"
    , OSC_I 11111
    ]

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
