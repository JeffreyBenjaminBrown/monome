{-# LANGUAGE DataKinds, ExtendedDefaultRules, OverloadedStrings #-}

module Mailbox where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)
import qualified Data.Map as M
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

playKey :: Synth BoopParams -> Press -> IO ()
playKey sy (Press x y p) = do
  set sy $ (toI $ 0.02 * fi (pressureToInt p) :: I "amp")
  set sy $ (toI $ 100 + x + 10*y :: I "freq")

mailboxSynths :: IO [OSC]
mailboxSynths = do
  s <- receivesAt "127.0.0.1" 11111
  acc <- newMVar []
  let loop :: IO [OSC]
      loop = do cmd <- getChar
                case cmd of 'q' -> close s >> readMVar acc >>= return
                            _   -> loop
      places = [(a,b) | a <- [1..16], b <- [1..16]]
  voices <- M.fromList . zip places <$> mapM (synth boop) (replicate 256 ())
  mailbox <- forkIO $ forever $ do
    eOsc <- decodeOSC <$> recv s 4096
    case eOsc of Left text -> putStrLn . show $ text
                 Right osc -> let p@(Press x y s) = readPress osc
                              in playKey ((M.!) voices (x,y)) p
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
