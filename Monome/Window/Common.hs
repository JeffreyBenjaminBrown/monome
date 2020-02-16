{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE TupleSections
, DataKinds
, ScopedTypeVariables #-}

module Monome.Window.Common (
    ledBecause_toPitchClass -- ^ LitPitches -> LedBecause -> Maybe PitchClass
  , soundKey -- ^ St -> ((X,Y), Switch) -> IO ()
  , silence -- ^ St -> (X,Y) -> IO ()
  ) where

import Prelude hiding (pred)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Vivid hiding (pitch)

import Monome.Math31
import Monome.Synth.Boop
import Monome.Types.Button
import Monome.Types.Initial


-- Todo (#speed) Instead, keep a map from xy to pitchclass
ledBecause_toPitchClass :: LitPitches -> LedBecause -> Maybe PitchClass
ledBecause_toPitchClass m ldr =
  fst <$> mPair
  where
    mPair = listToMaybe
            $ filter (S.member ldr . snd)
            $ M.toList m

silence :: St -> (X,Y) -> IO ()
silence st xy = set ((M.!) (stVoices st) xy) (0 :: I "amp")

soundKey :: St -> ((X,Y), Switch) -> IO ()
soundKey st (xy, sw) = do
  let pitch = xyToEt31 xy - xyToEt31 (stXyShift st)
  case S.member xy $ S.map fst $ stSustained st of
    True -> return () -- it's already sounding due to sustain
    False -> case sw of
      False -> silence st xy
      True -> let
        freq :: Float = 100 * et31ToFreq pitch
        voice :: Synth BoopParams = (M.!) (stVoices st) xy
        in set voice ( toI freq   :: I "freq"
                     , toI $ 0.15 :: I "amp" )
