{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Monome.Window.Common (
    ledBecause_toPitchClass -- ^ LitPitches -> LedBecause -> Maybe PitchClass
  , silenceMsg              -- ^ (X,Y) -> SoundMsg
  , soundKeySt              -- ^ St -> ((X,Y), Switch) -> [SoundMsg]
  , updateVoice             -- ^ SoundMsg -> St -> St
  ) where

import           Prelude hiding (pred)
import           Control.Lens
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
silenceMsg xy = SoundMsg {
    _soundMsgVoiceId = xy
  , _soundMsgPitch = Nothing
  , _soundMsgVal = 0
  , _soundMsgParam = "amp" }

soundKeySt :: St -> ((X,Y), Switch) -> [SoundMsg]
soundKeySt st (xy, sw) = do
  let pitch = xyToEt31 xy - xyToEt31 (_stXyShift st)
  if S.member xy $ S.map fst $ _stSustained st
    then [] -- it's already sounding due to sustain
    else if sw
         then let freqMsg = SoundMsg { _soundMsgVoiceId = xy
                                     , _soundMsgPitch = Just pitch
                                     , _soundMsgVal = 100 * et31ToFreq pitch
                                     , _soundMsgParam = "freq" }
              in [ freqMsg
                 , freqMsg & soundMsgVal .~ 0.15
                           & soundMsgParam .~ "amp" ]
         else [silenceMsg xy]

updateVoice :: SoundMsg -> St -> St
updateVoice sdMsg st = let
  vid   :: VoiceId = _soundMsgVoiceId sdMsg
  param :: Param   = _soundMsgParam   sdMsg
  f     :: Float   = _soundMsgVal     sdMsg
  in st & case (_soundMsgPitch sdMsg) of
    Nothing -> id
    Just p -> stVoices . at vid . _Just
              %~ (voicePitch                     .~ p)
              .  (voiceParams . at param . _Just .~ f)
