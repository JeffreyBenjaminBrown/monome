{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Monome.Window.Common (
    ledBecause_toPitchClass -- ^ LitPitches -> LedBecause -> Maybe PitchClass
  , silenceMsg              -- ^ (X,Y) -> SoundMsg
  , keyMsg                  -- ^ St -> ((X,Y), Switch) -> [SoundMsg]
  , updateVoice             -- ^ SoundMsg -> St -> St
  , vid_to_pitch            -- ^ St -> VoiceId -> Either String PitchClass
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

-- todo ? this is only used so far in Window.Keyboard
keyMsg :: St -> ((X,Y), Switch) -> [SoundMsg]
keyMsg st (xy, sw) = do
  let pitch = xyToEt31_st st xy
  if maybe False (S.member xy) $ _stSustained st
    then [] -- it's already sounding due to sustain
    else if sw
         then let freqMsg = ParamMsg { _paramMsgVoiceId = xy
                                     , _paramMsgPitch = Just pitch
                                     , _paramMsgVal = 100 * et31ToFreq pitch
                                     , _paramMsgParam = "freq" }
              in [ SoundMsgCreate xy -- PITFALL: Must come first.
                 , SoundMsg $ freqMsg
                 , SoundMsg $ freqMsg & paramMsgVal .~ Config.voiceAmplitude
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
updateVoice (SoundMsgCreate vid) st =
  -- The message also goes to Types.Window,
  -- which creates the voice (using IO)
  st & stVoices %~ M.insert vid
  ( Voice { _voiceSynth = Left ()
          , _voicePitch = Config.initialPitch
          , _voiceParams = mempty } )
updateVoice (SoundMsgFree vid) st =
  -- The message also goes to Types.Window,
  -- which destroys the voice (using IO)
  st & stVoices %~ M.delete vid

vid_to_pitch :: St -> VoiceId -> Either String PitchClass
vid_to_pitch st vid = maybe
  (Left $ "vid_to_pitch: voiceId " ++ show vid ++ " not found")
  (Right . flip mod 31 . _voicePitch)
  $ M.lookup vid (_stVoices st)
