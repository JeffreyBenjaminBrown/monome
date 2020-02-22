{-# LANGUAGE DataKinds
, ScopedTypeVariables
, TupleSections
#-}

module Monome.Window.Sustain (
    handler
  , label
  , sustainWindow
  ) where

import           Control.Lens
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Monome.Math31
import           Monome.Types.Button
import           Monome.Types.Initial
import           Monome.Window.Common
import qualified Monome.Window.Keyboard as Kbd


label :: WindowId
label = "sustain window"

theButton :: (X,Y)
theButton = (0,15)

sustainWindow :: Window
sustainWindow = Window {
    windowLabel = label
  , windowContains = (==) theButton
  , windowInit = id
  , windowRoutine = handler
}

handler :: St
        -> ( (X,Y) -- ^ ignored, since the sustain window has only one button
           , Switch)
        -> St
handler    st    (_ , False)      = st
handler    st    (_,  True)      = let
  st1 = updateSt st
  kbdMsgs :: [LedMsg] =
    if not $ _stSustainOn st1
    then map ( (Kbd.label,) . (,False) ) $
         concatMap (pcToXys $ _stXyShift st) $
         get_pitchClassesToDarken st st1
    else []
  sdMsgs :: [SoundMsg] =
    if not $ _stSustainOn st1
    then map silenceMsg $ S.toList $ get_voicesToSilence st
    else []
  sustainButtonMsg = ( label
                     , (theButton, _stSustainOn st1) )
  st2 = st1 & stPending_Monome .~ ( sustainButtonMsg : kbdMsgs )
        & stPending_Vivid %~ (sdMsgs ++)
  in foldr updateVoice st2 sdMsgs

get_voicesToSilence :: St -> Set VoiceId
get_voicesToSilence oldSt =
    -- If a voice was sustained before sustain was released,
    -- and it is not fingered, it should be darkened.
    S.difference (S.map fst $ _stSustained oldSt)
                 (S.fromList $ M.keys $ _stFingers oldSt)

get_pitchClassesToDarken :: St -> St -> Set PitchClass
  -- TODO ? speed: This calls `get_voicesToSilence`.
  -- Would it be faster to pass the result of `get_voicesToSilence`
  -- as a precomputed argument? (I'm guessing the compiler fogures it out.)
get_pitchClassesToDarken oldSt newSt =
  -- `get_pitchClassesToDarken` is nearly equal to `get_voicesToSilence`,
  -- but it excludes visual anchors as well as fingered notes.
  S.filter (not . mustStayLit) $ voicesToSilence_pcs
  where
    mustStayLit :: PitchClass -> Bool
    mustStayLit pc = case M.lookup pc $ _stLit newSt of
      Nothing -> False
      Just s -> if null s
        then error "Sustain handler: null value in LitPitches."
        else True
    voicesToSilence_pcs :: Set PitchClass =
      S.map snd $ S.filter f $ _stSustained oldSt
      where f :: (VoiceId, PitchClass) -> Bool
            f (b,_) = S.member b $ get_voicesToSilence oldSt

-- | When the sustain button is toggled --
-- which happens only when it is pressed, not when it is released --
-- the sustainOn value flips, the set of sustained pitches changes,
-- and the set of lit keys gains new reasons to be lit.
updateSt :: St -> St
updateSt st = let
  sustainOn' :: Bool = -- new sustain state
    not $ _stSustainOn st
  sustained' :: Set (VoiceId, PitchClass) = -- new sustained pitches
    if not sustainOn' then S.empty
    else S.fromList $ M.elems $ _stFingers st

  lit' | sustainOn' =
         foldr insertOneSustainedNote (_stLit st)
         $ map snd $ M.elems $ _stFingers st
       | otherwise =
         foldr deleteOneSustainedNote (_stLit st)
         $ S.toList $ S.map snd $ _stSustained st
  in st { _stSustainOn = sustainOn'
        , _stSustained = sustained'
        , _stLit       = lit'      }

-- | When sustain is toggled, the reasons for having LEDs on change.
-- If it is turned on, some LEDs are now lit for two reasons:
-- fingers and sustain. If it is turned off,
-- sustain is no longer a reason to light up any LEDs.
insertOneSustainedNote, deleteOneSustainedNote
  :: PitchClass -> LitPitches -> LitPitches
insertOneSustainedNote pc m =
  let reasons :: Set LedBecause =
        maybe S.empty id $ M.lookup pc m
  in M.insert pc (S.insert LedBecauseSustain reasons) m
deleteOneSustainedNote pc m =
  case M.lookup pc m of
    Nothing -> m -- TODO ? Should this throw an error? It shouldn't happen.
    Just reasons -> let
      reasons' = S.delete LedBecauseSustain reasons
      in if null reasons'
         then M.delete pc m
         else M.insert pc reasons' m
