{-# LANGUAGE DataKinds
, ScopedTypeVariables
#-}

module Monome.Window.Sustain (
    sustainWindow
  , label
  ) where

import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Set as S
import Vivid

import Monome.Math31
import Monome.Types.Window
import Monome.Types.Button
import Monome.Types.State


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
handler _   _  _ (_ , SwitchOff) = return ()
handler mst toSustainWindow _ (xy0, SwitchOn) = do
  st <- takeMVar mst -- PITFALL: old state; has opposite `stSustainOn` value
  let sustainOn' :: Bool = not $ stSustainOn st
      sustained' :: S.Set ((X,Y), PitchClass) =
        if not sustainOn' then S.empty
        else S.fromList $ M.toList $ stFingers st

  -- redraw the sustain window, silence anything that needs it
  let drawSustainWindow = curry toSustainWindow xy0
  case sustainOn' of
    False -> do -- Turn sustain off.
      -- TODO : Un-draw sustained pitches not under fingers.
      let quiet xy = set ((M.!) (stVoices st) xy) (0 :: I "amp")
          sustainedAndNotFingered = S.difference
            (S.map fst $ stSustained st)
            (S.fromList $ M.keys $ stFingers st)
        in mapM_ quiet sustainedAndNotFingered -- Silence some voices.
      drawSustainWindow LedOff -- Darken the sustain button.
    True -> drawSustainWindow LedOn

  let lit' | sustainOn' =
             foldr insertOneSustainedNote (stLit st)
             $ M.toList $ stFingers st
           | otherwise =
             foldr deleteOneSustainedNote (stLit st)
             $ S.toList $ stSustained st
      st' = st { stSustainOn = sustainOn'
               , stSustained = sustained'
               , stLit       = lit'      }
  putMVar mst st'

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
