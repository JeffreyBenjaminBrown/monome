{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Monome.Window.Common (
    ledBecause_toPitchClass -- ^ LitPitches -> LedBecause -> Maybe PitchClass
  , silenceMsg              -- ^ (X,Y) -> SoundMsg
  , keyOnMsg                -- ^ St -> ((X,Y), Switch) -> [SoundMsg]
  , updateVoice             -- ^ SoundMsg -> St -> St
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S

import qualified Monome.Config as Config
import           Monome.Math31
import           Monome.Types.Button
import           Monome.Types.Initial


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

keyOnMsg :: St -> ((X,Y), Switch) -> [SoundMsg]
keyOnMsg st (xy, sw) = do
  let pitch = xyToEt31_st st xy
  if S.member xy $ S.map fst $ _stSustained st
    then [] -- it's already sounding due to sustain
    else if sw
         then let msg = SoundMsg { _soundMsgVoiceId = xy
                                 , _soundMsgPitch = Just pitch }
              in [ msg & soundMsgVal .~ 100 * et31ToFreq pitch
                       & soundMsgParam .~ "freq"
                 , msg & soundMsgVal .~ Config.voiceAmplitude
                       & soundMsgParam .~ "amp" ]
         else [silenceMsg xy]

updateVoice :: SoundMsg -> St -> St
updateVoice sdMsg st = let
  vid   :: VoiceId = _soundMsgVoiceId sdMsg
  param :: Param   = _soundMsgParam   sdMsg
  f     :: Float   = _soundMsgVal     sdMsg
  in st & case _soundMsgPitch sdMsg of
            Nothing -> id
            Just p -> stVoices . at vid . _Just
                      %~ (voicePitch                     .~ p)
                      .  (voiceParams . at param . _Just .~ f)
