{-# LANGUAGE DataKinds, ExtendedDefaultRules, OverloadedStrings #-}

module ET31 where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Monad (forever)
import qualified Data.Map as M
import Vivid
import Vivid.OSC

import Util.Byte
import Util.Network
import Types.Button


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
  set sy $ (toI $ 0.05 * fi (pressureToInt p) :: I "amp")
  set sy $ (toI $ 100 * et31ToFreq (xyToEt31 pr) :: I "freq")

enharmonicKeys :: (X,Y) -> [(X,Y)]
enharmonicKeys (x,y) = let contained x = x <= 15 && x >= 0
  in filter (\(x,y) -> contained x && contained y)
     $ [(x + x' * 6 + y' * (-1), y + x' * 1 + y' * 5 )
       | x' <- [-2..2], y' <- [-3..3] ]

et31 :: IO ()
et31 = do
  inbox <- receivesAt "127.0.0.1" 11111
  toMonome <- sendsTo (unpack localhost) 13993
  mapM (send toMonome . shineToOscByte "/monome"
        . (\(x,y) -> Shine x y Lit)) $ enharmonicKeys (8,8)
  let places = [(a,b) | a <- [0..15], b <- [0..15]]
  voices <- M.fromList . zip places <$> mapM (synth boop) (replicate 256 ())
  mailbox <- forkIO $ forever $ do
    eOsc <- decodeOSC <$> recv inbox 4096
    case eOsc of Left text -> putStrLn . show $ text
                 Right osc -> let p@(Press x y s) = readPress osc
                              in playKey ((M.!) voices (x,y)) p
  let loop :: IO ()
      loop = do cmd <- getChar
                case cmd of 'q' -> close inbox
                                   >> mapM_ free (M.elems voices)
                                   >> killThread mailbox
                            _   -> loop
  loop
