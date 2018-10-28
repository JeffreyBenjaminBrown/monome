{-# LANGUAGE DataKinds #-}

module Window.Keyboard (
  keyboardWindow
  , label
  ) where

import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Set as S
import Vivid

import Synth
import Types.Window
import Types.Button
import Types.State
import Util.Byte
import Math31
import Window.Common


label = "keyboard window"

keyboardWindow =  Window {
  windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInit = \mst toKeyboard -> do
      st <- readMVar mst
      mapM_ (drawPitchClass toKeyboard (xyShift st) LedOn) $ M.keys $ lit st
  , windowHandler = handler }

soundKey :: State -> ((X,Y), Switch) -> IO ()
soundKey st (xy, sw)
  | S.member xy (sustained st) = return ()
  | otherwise =
    let freq = 100 * (et31ToFreq
                      $ xyToEt31 xy - xyToEt31 (xyShift st))
        voice = (M.!) (voices st) xy
    in set voice ( toI freq                         :: I "freq"
                 , toI $ 0.15 * fi (switchToInt sw) :: I "amp" )

handler :: MVar State
        -> LedRelay
        -> [Window]
        -> ((X,Y), Switch)
        -> IO ()
handler mst toKeyboard _ press @ (xy,sw) = do
  st <- takeMVar mst
  soundKey st press
  let newFingers = case sw of
        SwitchOn -> S.insert xy $ fingers st
        SwitchOff -> S.delete xy $ fingers st
      pitchClassNow = mod (xyToEt31 $ addPair xy $ negPair $ xyShift st) 31
        -- what that key represents currently.
      pitchClassBefore = dependentPitchClass (lit st) xy
        -- pitches that key lit up in the past
      nl = newLit (xy,sw) pitchClassNow pitchClassBefore $ lit st
      oldKeys = S.fromList $ M.keys $ lit st
      newKeys = S.fromList $ M.keys $ nl
      toDark = S.difference oldKeys newKeys
      toLight = S.difference newKeys oldKeys
  mapM_ (drawPitchClass toKeyboard (xyShift st) LedOff) toDark
  mapM_ (drawPitchClass toKeyboard (xyShift st) LedOn) toLight
  putMVar mst $ st { fingers = newFingers
                   , lit = nl }

newLit :: ((X,Y), Switch)
       -> PitchClass -- ^ what xy represents now
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
      in case S.size reasons < 2 of -- size < 1 should not happen
        True -> M.delete pc m
        False -> M.insert pc (S.delete  (LedFromSwitch xy) reasons) m
