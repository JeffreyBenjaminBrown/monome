{-# LANGUAGE OverloadedStrings #-}

module Device where

import Vivid.OSC

import Util.Byte


data Device = Device { deviceName :: ByteString
                     , deviceType :: ByteString
                     , devicePort :: Int } deriving (Show, Eq, Ord)

readDevice :: OSC -> Device
readDevice ( OSC "/serialosc/device" [ OSC_S name
                                     , OSC_S monomeType
                                     , OSC_I port ] )
  = Device { deviceName = name
           , deviceType = monomeType
           , devicePort = fromIntegral port }
