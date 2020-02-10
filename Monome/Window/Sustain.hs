{-# LANGUAGE DataKinds
, ScopedTypeVariables
#-}

module Monome.Window.Sustain (
    sustainWindow
  , label
  ) where

import Control.Concurrent.MVar
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import Vivid

import           Monome.Math31
import           Monome.Types.Window
import           Monome.Types.Button
import           Monome.Types.State
import           Monome.Window.Common (drawPitchClass)
import qualified Monome.Window.Keyboard as Kbd


label :: WindowLabel
label = "sustain window"

theButton :: (X,Y)
theButton = (0,15)

sustainWindow :: Window
sustainWindow = Window {
    windowLabel = label
  , windowContains = (==) theButton
  , windowInit = \_ _ -> return ()
  , windowRoutine = handler
}

handler :: MVar St -> LedRelay -> [Window] -> ((X,Y), Switch) -> IO ()
handler    _          _           _           (_ , False) = return ()
handler    mst        toSustain   ws          (xy0, True) = do
  st <- takeMVar mst
  let st' = updateSt st
  putMVar mst st'
  case stSustainOn st' of

    False -> do -- Turn sustain off.
      let voicesToSilence      = get_voicesToSilence st
          pitchClassesToDarken = get_pitchClassesToDarken st st'
      let -- Silence some voices.
        silence xy = set ((M.!) (stVoices st) xy) (0 :: I "amp")
        in mapM_ silence voicesToSilence
      let -- Darken some of the keyboard (which is a different window).
        draw = drawPitchClass toKeyboard $ stXyShift st
          where toKeyboard = relayToWindow st Kbd.label ws
        in mapM_ (draw False) $ S.toList pitchClassesToDarken
      curry toSustain xy0 False -- Darken the sustain button.

    True -> -- Turn sustain on.
      curry toSustain xy0 True

get_voicesToSilence :: St -> Set (X,Y)
get_voicesToSilence oldSt =
    -- If a voice was sustained before sustain was released,
    -- and it is not fingered, it should be darkened.
    S.difference (S.map fst $ stSustained oldSt)
                 (S.fromList $ M.keys $ stFingers oldSt)

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
    mustStayLit pc = case M.lookup pc $ stLit newSt of
      Nothing -> False
      Just s -> if null s
        then error "Sustain handler: null value in LitPitches."
        else True
    voicesToSilence_pcs :: Set PitchClass =
      S.map snd $ S.filter f $ stSustained oldSt
      where f :: ((X,Y), PitchClass) -> Bool
            f (b,_) = S.member b $ get_voicesToSilence oldSt

-- | When the sustain button is toggled --
-- which happens only when it is pressed, not when it is released --
-- the sustainOn value flips, the set of sustained pitches changes,
-- and the set of lit keys gains new reasons to be lit.
updateSt :: St -> St
updateSt st = let
  sustainOn' :: Bool = -- new sustain state
    not $ stSustainOn st
  sustained' :: Set ((X,Y), PitchClass) = -- new sustained pitches
    if not sustainOn' then S.empty
    else S.fromList $ M.toList $ stFingers st

  lit' | sustainOn' =
         foldr insertOneSustainedNote (stLit st)
         $ M.elems $ stFingers st
       | otherwise =
         foldr deleteOneSustainedNote (stLit st)
         $ S.toList $ S.map snd $ stSustained st
  in st { stSustainOn = sustainOn'
        , stSustained = sustained'
        , stLit       = lit'      }

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
