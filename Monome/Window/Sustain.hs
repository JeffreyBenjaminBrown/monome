{-# LANGUAGE DataKinds
, LambdaCase
, ScopedTypeVariables
, TupleSections
#-}

module Monome.Window.Sustain (
    handler
  , label
  , sustainWindow
  , theButton

  , voicesToSilence_uponSustainOff -- ^ St -> Set VoiceId
  , toggleSustain                  -- ^ St -> St
  , insertOneSustainedNote -- ^ PitchClass -> LitPitches -> LitPitches
  , deleteOneSustainedNote -- ^ PitchClass -> LitPitches -> LitPitches
  ) where

import           Control.Lens
import           Data.Either.Combinators (mapLeft, mapBoth)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
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
  , windowHandler = handler
}

handler :: ( (X,Y) -- ^ ignored, since the sustain window has only one button
           , Switch)
        -> St -> Either String St
handler (_ , False) st = Right st
handler (_,  True)  st = mapLeft ("Sustain.handler: " ++) $ do
  st1 :: St <- toggleSustain st
  pcsToDarken :: [PitchClass] <- pitchClassesToDarken_uponSustainOff st st1
  let vsToSilence :: Set VoiceId = voicesToSilence_uponSustainOff st
      kbdMsgs :: [LedMsg] =
        if null $ _stSustained st1
        then map ( (Kbd.label,) . (,False) ) $
             concatMap (pcToXys $ _stXyShift st) $
             pcsToDarken
        else []
      sdMsgs :: [SoundMsg] =
        if null $ _stSustained st1
        then concatMap silenceMsg $ S.toList $
             vsToSilence
        else []
      sustainButtonMsg = ( label
                         , (theButton, isJust $ _stSustained st1) )
      st2 = st1 & stPending_Monome %~ flip (++) (sustainButtonMsg : kbdMsgs)
                & stPending_Vivid  %~ flip (++) sdMsgs
  Right $ foldr updateVoice st2 sdMsgs

pitchClassesToDarken_uponSustainOff
  :: St -> St -> Either String [PitchClass]
  -- todo ? speed: This calls `voicesToSilence_uponSustainOff`.
  -- Would it be faster to pass the result of `voicesToSilence_uponSustainOff`
  -- as a precomputed argument? (I'm guessing the compiler fogures it out.)
pitchClassesToDarken_uponSustainOff oldSt newSt =
  -- `pitchClassesToDarken_uponSustainOff` is nearly equal to `voicesToSilence_uponSustainOff`,
  -- but it excludes visual anchors as well as fingered notes.
  filter (not . mustStayLit) <$> voicesToSilence_pcs
  where
    mustStayLit :: PitchClass -> Bool
    mustStayLit pc = case M.lookup pc $ _stLit newSt of
      Nothing -> False
      Just s -> if null s
        then error "pitchClassesToDarken_uponSustainOff: null value in LitPitches."
        else True
    voicesToSilence_pcs :: Either String [PitchClass] =
      mapLeft ("pitchClassesToDarken_uponSustainOff: " ++)
      $ mapM (vid_to_pitch oldSt)
      $ S.toList $ voicesToSilence_uponSustainOff oldSt

voicesToSilence_uponSustainOff :: St -> Set VoiceId
voicesToSilence_uponSustainOff st = let
  sustained :: Set VoiceId =
    maybe mempty id $ _stSustained st
  fingered :: Set VoiceId =
    S.fromList $ M.keys $ _stFingers st
  in S.difference sustained fingered

-- | When the sustain button is toggled --
-- which happens only when it is pressed, not when it is released --
-- the set of sustained pitches changes
-- and the set of lit keys gains new reasons to be lit.
--
-- If sustain is off and nothing is fingered, sustain cannot be turned on.
toggleSustain :: St -> Either String St
toggleSustain st
  | null (_stSustained st) && null (_stFingers st) = Right st
    -- If sustain is off and nothing is fingered, sustain cannot be turned on.
  | otherwise = let
    sustainOnNow :: Bool =
      isNothing $ _stSustained st
    sustainedVs :: Maybe (Set VoiceId) = if sustainOnNow
      then Just $ S.fromList $ M.elems $ _stFingers st
      else Nothing

    lit' :: Either String (Map PitchClass (Set LedBecause))
    lit' | sustainOnNow = let
           epcs :: Either String [PitchClass] =
             mapM (vid_to_pitch st)
             $ M.elems $ _stFingers st
           in mapBoth
              ("toggleSustain: " ++)
              (foldr insertOneSustainedNote $ _stLit st)
              epcs
         | otherwise = let
           susVs :: Set VoiceId =
             maybe (error "impossible") id $ _stSustained st
           epcs :: Either String [PitchClass] =
             mapM (vid_to_pitch st) $ S.toList susVs
           in mapBoth
              ("toggleSustain: " ++)
              (foldr deleteOneSustainedNote $ _stLit st)
              epcs
    in case lit' of
      Left s -> Left s
      Right l -> Right st { _stSustained = sustainedVs
                          , _stLit       = l }

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
    Nothing -> m -- todo ? Should this throw an error? It shouldn't happen.
    Just reasons -> let
      reasons' = S.delete LedBecauseSustain reasons
      in if null reasons'
         then M.delete pc m
         else M.insert pc reasons' m
