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
    False -> do -- Turn sustain off: Free some voices, dark the led.
      let quiet xy = set ((M.!) (stVoices st) xy) (0 :: I "amp")
          sustainedAndNotFingered = S.difference
            (S.map fst $ stSustained st)
            (S.fromList $ M.keys $ stFingers st)
        in mapM_ quiet sustainedAndNotFingered
      drawSustainWindow LedOff
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

insertOneSustainedNote :: ((X,Y), PitchClass)
                       -> M.Map PitchClass (S.Set LedBecause)
                       -> M.Map PitchClass (S.Set LedBecause)
insertOneSustainedNote (xy, pc) m
  | M.lookup pc m == Nothing =
      M.insert pc (S.singleton $ LedBecauseSustain xy) m
  | Just reasons <- M.lookup pc m =
      M.insert pc (S.insert (LedBecauseSustain xy) reasons) m
  | otherwise = error "insertOneSustainedNote: should be impossible."

deleteOneSustainedNote :: ((X,Y), PitchClass)
                       -> M.Map PitchClass (S.Set LedBecause)
                       -> M.Map PitchClass (S.Set LedBecause)
deleteOneSustainedNote (xy, pc) m =
  case M.lookup pc m of
    Nothing -> m -- TODO ? Should this throw an error? It shouldn't happen.
    Just reasons ->
      -- TODO (#safety) Check that that's really what's being deleted.
      case S.size reasons < 2 of -- size < 1 should not happen
        True -> M.delete pc m
        False -> M.insert pc (S.delete (LedBecauseSustain xy) reasons) m
