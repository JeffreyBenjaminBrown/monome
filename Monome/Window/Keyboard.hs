{-# LANGUAGE DataKinds #-}

module Monome.Window.Keyboard (
  keyboardWindow
  , label
  ) where

import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Set as S
import Vivid

import Monome.Types.Window
import Monome.Types.Button
import Monome.Types.State
import Monome.Util.Byte
import Monome.Math31
import Monome.Window.Common


label = "keyboard window"

keyboardWindow =  Window {
  windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInit = \mst toKeyboard -> do
      st <- readMVar mst
      mapM_ (drawPitchClass toKeyboard (xyShift st) LedOn) $ M.keys $ lit st
  , windowHandler = handler }

handler :: MVar State
        -> LedRelay
        -> [Window]
        -> ((X,Y), Switch)
        -> IO ()
handler mst toKeyboard _ press @ (xy,sw) = do
  st <- takeMVar mst
  soundKey st press
  let
      pitchClassNow = mod (xyToEt31 $ addPair xy $ negPair $ xyShift st) 31
        -- what that key represents currently.
      pitchClassThen = dependentPitchClass (lit st) $ LedFromSwitch xy
        -- pitches that key lit up in the past
      fingers' = case sw of
        SwitchOn -> M.insert xy pitchClassNow $ fingers st
        SwitchOff -> M.delete xy $ fingers st
      nl = newLit (xy,sw) pitchClassNow pitchClassThen $ lit st
      oldKeys = S.fromList $ M.keys $ lit st
      newKeys = S.fromList $ M.keys $ nl
      toDark = S.difference oldKeys newKeys
      toLight = S.difference newKeys oldKeys
  -- putStrLn . show $ fingers' -- handy spot to print
  mapM_ (drawPitchClass toKeyboard (xyShift st) LedOff) toDark
  mapM_ (drawPitchClass toKeyboard (xyShift st) LedOn) toLight
  putMVar mst $ st { fingers = fingers'
                   , lit = nl }

soundKey :: State -> ((X,Y), Switch) -> IO ()
soundKey st (xy, sw) = do
  let pitchClass = xyToEt31 xy - xyToEt31 (xyShift st)
  case S.member xy $ S.map fst $ sustained st of
    True -> return () -- it's already sounding due to sustain
    False ->
      let freq = 100 * et31ToFreq pitchClass
          voice = (M.!) (voices st) xy
      in set voice ( toI freq                         :: I "freq"
                   , toI $ 0.15 * fi (switchToInt sw) :: I "amp" )

newLit :: ((X,Y), Switch)
       -> PitchClass       -- ^ what xy represents now
       -> Maybe PitchClass -- ^ what xy represented when it was pressed
       -> M.Map PitchClass (S.Set LedReason)
       -> M.Map PitchClass (S.Set LedReason)
newLit (xy,SwitchOn) pcNow _ m
  | M.lookup pcNow m == Nothing =
      M.insert pcNow (S.singleton $ LedFromSwitch xy) m
  | Just reasons <- M.lookup pcNow m =
      M.insert pcNow (S.insert (LedFromSwitch xy) reasons) m
newLit (xy,SwitchOff) _ mpcThen m
  | mpcThen == Nothing = m
  | Just pc <- mpcThen = let Just reasons = M.lookup pc m
      -- TODO (#safety) Check that that's really what's being deleted.
      in case S.size reasons < 2 of -- size < 1 should not happen
        True -> M.delete pc m
        False -> M.insert pc (S.delete (LedFromSwitch xy) reasons) m
