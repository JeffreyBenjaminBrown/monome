{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Monome.Window.Common (
    ledBecause_toPitchClass -- ^ LitPitches -> LedBecause -> Maybe PitchClass
  , silenceMsg              -- ^ (X,Y) -> SoundMsg
  , keyMsg                  -- ^ St -> ((X,Y), Switch) -> [SoundMsg]
  , updateVoice             -- ^ SoundMsg -> St -> St
  , vid_to_pitch            -- ^ St -> VoiceId -> PitchClass
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


-- todo (#speed) Instead, keep a map from xy to pitchclass
ledBecause_toPitchClass :: LitPitches -> LedBecause -> Maybe PitchClass
ledBecause_toPitchClass m ldr =
  fst <$> mPair
  where
    mPair = listToMaybe
            $ filter (S.member ldr . snd)
            $ M.toList m

silenceMsg :: (X,Y) -> [SoundMsg]
silenceMsg xy =
  [ SoundMsg $ ParamMsg { _paramMsgVoiceId = xy
                        , _paramMsgPitch = Nothing
                        , _paramMsgVal = 0
                        , _paramMsgParam = "amp" }
  , SoundMsgFree xy ] -- PITFALL: Should come last.

keyMsg :: St -> ((X,Y), Switch) -> [SoundMsg]
keyMsg st (xy, sw) = do
  let pitch = xyToEt31_st st xy
  if maybe False (S.member xy) $ _stSustained st
    then [] -- it's already sounding due to sustain
    else if sw
         then let pm = ParamMsg { _paramMsgVoiceId = xy
                                , _paramMsgPitch = Just pitch }
              in [ SoundMsgCreate xy -- PITFALL: Must come first.
                 , SoundMsg $ pm & paramMsgVal .~ 100 * et31ToFreq pitch
                                 & paramMsgParam .~ "freq"
                 , SoundMsg $ pm & paramMsgVal .~ Config.voiceAmplitude
                                 & paramMsgParam .~ "amp" ]
         else silenceMsg xy

updateVoice :: SoundMsg -> St -> St
updateVoice (SoundMsg sdMsg) st = let
  vid   :: VoiceId = _paramMsgVoiceId sdMsg
  param :: Param   = _paramMsgParam   sdMsg
  f     :: Float   = _paramMsgVal     sdMsg
  in st & case _paramMsgPitch sdMsg of
            Nothing -> id
            Just p -> stVoices . at vid . _Just
                      %~ (voicePitch                     .~ p)
                      .  (voiceParams . at param . _Just .~ f)
updateVoice _ st = st -- Making or freeing voices is handled in Types.Window,
                      -- because making a voice requires IO.

vid_to_pitch :: St -> VoiceId -> PitchClass
vid_to_pitch st v = maybe
  (error "vid_to_pitch: voice not found")
  (flip mod 31 . _voicePitch)
  $ M.lookup v (_stVoices st)
