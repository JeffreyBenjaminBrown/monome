{-# LANGUAGE DataKinds #-}

module Monome.Window.Sustain (
  sustainWindow
  , label
  ) where

import Control.Concurrent.MVar
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Vivid

import Monome.Math31
import Monome.Types.Window
import Monome.Types.Button
import Monome.Types.State
import Monome.Util.Network
import Monome.Window.Common
import qualified Monome.Window.Keyboard


label = "sustain window"

theButton = (0,15)

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

handler :: MVar State -> LedRelay -> [Window] -> ((X,Y), Switch) -> IO ()
handler _   _  _ (_ , SwitchOff) = return ()
handler mst toSustainWindow _ (xy, SwitchOn ) = do
  st <- takeMVar mst -- PITFALL: old state, opposite value of `sustainOn`
  let sustainOn' = not $ sustainOn st
      sustained' = if not sustainOn' then S.empty
                   else S.fromList $ M.toList $ fingers st

  -- redraw the sustain window, silence anything that needs it
  let drawSustainWindow = curry toSustainWindow xy
  case sustainOn' of
    False -> do -- Turn sustain off: Free some voices, dark the led.
      let quiet xy = set ((M.!) (voices st) xy) (0 :: I "amp")
          sustainedAndNotFingered = S.difference (S.map fst $ sustained st)
                                    (S.fromList $ M.keys $ fingers st)
      drawSustainWindow LedOff
      mapM_ quiet $ sustainedAndNotFingered

    True -> do
      drawSustainWindow LedOn

  let lit' | sustainOn' =
             foldr insertOneSustainedNote (lit st) $ M.toList $ fingers st
           | not sustainOn' =
             foldr deleteOneSustainedNote (lit st) $ S.toList $ sustained st
      st' = st { sustainOn = sustainOn'
               , sustained = sustained'
               , lit       = lit'      }

  putMVar mst st'
