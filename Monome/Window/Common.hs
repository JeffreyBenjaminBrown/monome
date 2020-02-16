{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Monome.Window.Common (
    ledBecause_toPitchClass -- ^ LitPitches -> LedBecause -> Maybe PitchClass
  , silenceMsg              -- ^ (X,Y) -> SoundMsg
  , soundKeySt              -- ^ St -> ((X,Y), Switch) -> [SoundMsg]
  ) where

import Prelude hiding (pred)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S

import Monome.Math31
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

silenceMsg :: (X,Y) -> SoundMsg
silenceMsg xy = (xy, 0, "amp")

soundKeySt :: St -> ((X,Y), Switch) -> [SoundMsg]
soundKeySt st (xy, sw) = do
  let pitch = xyToEt31 xy - xyToEt31 (stXyShift st)
  if S.member xy $ S.map fst $ stSustained st
    then [] -- it's already sounding due to sustain
    else if sw
         then [ (xy, 100 * et31ToFreq pitch, "freq")
              , (xy, 0.15                  , "amp") ]
         else [silenceMsg xy]
