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
  sin <- tanh' $ sinOsc (freq_ (V::V "freq"))
  s1 <- (V::V "amp") ~* sin ~* sin
  out 0 [s1, s1]

xyToEt31 :: Press -> Float
xyToEt31 (Press x y _) = fi (15-x) + 6 * fi y

et31ToFreq :: Float -> Float
et31ToFreq f = 2**(f/31)

playKey :: Synth BoopParams -> Press -> IO ()
playKey sy pr@(Press x y p) = do
  set sy $ (toI $ 0.1 * fi (pressureToInt p) :: I "amp")
  set sy $ (toI $ 50 * et31ToFreq (xyToEt31 pr) :: I "freq")


mailboxSynths :: IO ()
mailboxSynths = do
  s <- receivesAt "127.0.0.1" 11111
  let places = [(a,b) | a <- [0..15], b <- [0..15]]
  voices <- M.fromList . zip places <$> mapM (synth boop) (replicate 256 ())
  let loop :: IO ()
      loop = do cmd <- getChar
                case cmd of 'q' -> do close s >> mapM_ free (M.elems voices)
                            _   -> loop
  mailbox <- forkIO $ forever $ do
    eOsc <- decodeOSC <$> recv s 4096
    case eOsc of Left text -> putStrLn . show $ text
                 Right osc -> let p@(Press x y s) = readPress osc
                              in playKey ((M.!) voices (x,y)) p
  loop
