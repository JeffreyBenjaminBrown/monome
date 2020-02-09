{-# LANGUAGE DataKinds #-}

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
  , windowContains = \(x,y) -> x == fst theButton && y == snd theButton
  , windowInit = \_ _ -> return ()
  , windowHandler = handler
}

insertOneSustainedNote :: ((X,Y), PitchClass)
                       -> M.Map PitchClass (S.Set LedReason)
                       -> M.Map PitchClass (S.Set LedReason)
insertOneSustainedNote (xy, pc) m
  | M.lookup pc m == Nothing =
      M.insert pc (S.singleton $ LedFromSustain xy) m
  | Just reasons <- M.lookup pc m =
      M.insert pc (S.insert (LedFromSustain xy) reasons) m
  | otherwise = error "insertOneSustainedNote: should be impossible."

deleteOneSustainedNote :: ((X,Y), PitchClass)
                       -> M.Map PitchClass (S.Set LedReason)
                       -> M.Map PitchClass (S.Set LedReason)
deleteOneSustainedNote (xy, pc) m
  | M.lookup pc m == Nothing = m -- should not happen
  | Just reasons <- M.lookup pc m =
      -- TODO (#safety) Check that that's really what's being deleted.
      case S.size reasons < 2 of -- size < 1 should not happen
        True -> M.delete pc m
        False -> M.insert pc (S.delete (LedFromSustain xy) reasons) m
  | otherwise = error "deleteOneSustainedNote: should be impossible."

handler :: MVar State -> LedRelay -> [Window] -> ((X,Y), Switch) -> IO ()
handler _   _  _ (_ , SwitchOff) = return ()
handler mst toSustainWindow _ (xy0, SwitchOn ) = do
  st <- takeMVar mst -- PITFALL: old state, opposite value of `sustainOn`
  let sustainOn' = not $ stSustainOn st
      sustained' = if not sustainOn' then S.empty
                   else S.fromList $ M.toList $ stFingers st

  -- redraw the sustain window, silence anything that needs it
  let drawSustainWindow = curry toSustainWindow xy0
  case sustainOn' of
    False -> do -- Turn sustain off: Free some voices, dark the led.
      let quiet xy = set ((M.!) (stVoices st) xy) (0 :: I "amp")
          sustainedAndNotFingered = S.difference (S.map fst $ stSustained st)
                                    (S.fromList $ M.keys $ stFingers st)
      drawSustainWindow LedOff
      mapM_ quiet $ sustainedAndNotFingered

    True -> do
      drawSustainWindow LedOn

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
