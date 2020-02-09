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

handler :: MVar State -> LedRelay -> [Window] -> ((X,Y), Switch) -> IO ()
handler    _             _           _           (_ , False) = return ()
handler    mst           toSustain   ws          (xy0, True) = do
  st <- takeMVar mst
  let sustainOn' :: Bool = -- new sustain state
        not $
        stSustainOn st -- old sustain state
      sustained' :: Set ((X,Y), PitchClass) = -- new sustained pitches
        if not sustainOn' then S.empty
        else S.fromList $ M.toList $ stFingers st
      drawSustainWindow = curry toSustain xy0

      lit' | sustainOn' =
             foldr insertOneSustainedNote (stLit st)
             $ M.toList $ stFingers st
           | otherwise =
             foldr deleteOneSustainedNote (stLit st)
             $ S.toList $ stSustained st
      st' = st { stSustainOn = sustainOn'
               , stSustained = sustained'
               , stLit       = lit'      }
  putMVar mst st'

  case sustainOn' of -- IO: lights and sound
    False -> do -- Turn sustain off.
      let sustainedAndNotFingeredXYs :: Set (X,Y) =
            S.difference (S.map fst $ stSustained st)
                         (S.fromList $ M.keys $ stFingers st)
          sustained_notFingered_notAnchored_PCs :: Set PitchClass =
            S.map snd $ S.filter f $ stSustained st where
              f :: ((X,Y), PitchClass) -> Bool
              f (b,_) = S.member b sustainedAndNotFingeredXYs

      -- Silence some voices.
      let quiet xy = set ((M.!) (stVoices st) xy) (0 :: I "amp")
      mapM_ quiet sustainedAndNotFingeredXYs

      -- Darken some keys on the keyboard (which is a different window).
      let keyboard = maybe err id $ findWindow ws Kbd.label where
            err = error "Window.Shift.handler: keyboard window not found."
          toKeyboard = relayIfHere (stToMonome st) ws keyboard
          draw = drawPitchClass toKeyboard $ stXyShift st
      mapM_ (draw False) $ S.toList sustained_notFingered_notAnchored_PCs

      -- Darken the sustain button.
      drawSustainWindow False

    True -> drawSustainWindow True

insertOneSustainedNote, deleteOneSustainedNote
  :: ((X,Y), PitchClass) -> LitPitches -> LitPitches
insertOneSustainedNote (xy, pc) m =
  case M.lookup pc m of
    Nothing ->      M.insert pc (S.singleton $ LedBecauseSustain xy) m
    Just reasons -> M.insert pc (S.insert (LedBecauseSustain xy) reasons) m
deleteOneSustainedNote (xy, pc) m =
  case M.lookup pc m of
    Nothing -> m -- TODO ? Should this throw an error? It shouldn't happen.
    Just reasons ->
      -- TODO (#safety) Check that that's really what's being deleted.
      case S.size reasons < 2 of -- TODO ? verify size < 1 (should not happen)
        True -> M.delete pc m
        False -> M.insert pc (S.delete (LedBecauseSustain xy) reasons) m
