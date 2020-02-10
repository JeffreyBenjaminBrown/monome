{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
, ScopedTypeVariables
#-}

module Monome.Window.Keyboard (
    keyboardWindow
  , label
  ) where

import Prelude hiding (pred)
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Set (Set)
import Vivid hiding (pitch)

import Monome.Types.Window
import Monome.Types.Button
import Monome.Types.State
import Monome.Util
import Monome.Math31
import Monome.Window.Common


label :: WindowLabel
label = "keyboard window"

keyboardWindow :: Window
keyboardWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInit = \mst toKeyboard -> do
      st <- readMVar mst
      mapM_ (drawPitchClass toKeyboard (stXyShift st) True)
        $ M.keys $ stLit st
  , windowRoutine = handler }

handler :: MVar St
        -> LedRelay
        -> [Window]
        -> ((X,Y), Switch)
        -> IO ()
handler mst toKeyboard _ press @ (xy,sw) = do
  st <- takeMVar mst
  soundKey st press

  let pcNow :: PitchClass =
        mod (xyToEt31 $ addPair xy $ negPair $ stXyShift st) 31
        -- what the key represents currently
      pcThen :: Maybe PitchClass =
        ledBecause_toPitchClass (stLit st) $ LedBecauseSwitch xy
        -- what the key represented when it was pressed,
        -- if it is now being released
      fingers' = case sw of
        True  -> M.insert xy pcNow $ stFingers st
        False -> M.delete xy $ stFingers st
      lit' :: LitPitches = updateStLit (xy,sw) pcNow pcThen $ stLit st
      oldKeys :: Set PitchClass = S.fromList $ M.keys $ stLit st
      newKeys :: Set PitchClass = S.fromList $ M.keys $ lit'
      toDark  :: Set PitchClass = S.difference oldKeys newKeys
      toLight :: Set PitchClass = S.difference newKeys oldKeys

  mapM_ (drawPitchClass toKeyboard (stXyShift st) False) toDark
  mapM_ (drawPitchClass toKeyboard (stXyShift st) True)  toLight
  putMVar mst $ st { stFingers = fingers'
                   , stLit = lit' }

soundKey :: St -> ((X,Y), Switch) -> IO ()
soundKey st (xy, sw) = do
  let pitch = xyToEt31 xy - xyToEt31 (stXyShift st)
  case S.member xy $ stSustained st of
    True -> return () -- it's already sounding due to sustain
    False ->
      let freq = 100 * et31ToFreq pitch
          voice = (M.!) (stVoices st) xy
      in set voice ( toI freq                  :: I "freq"
                   , toI $ 0.15 * boolToInt sw :: I "amp" )


updateStLit :: ((X,Y), Switch)
       -> PitchClass       -- ^ what xy represents now
       -> Maybe PitchClass -- ^ what xy represented when it was last pressed
       -> LitPitches
       -> LitPitches

-- | When a button is newly pressed,
-- it adds anoother LedBecause to the LitPitches.
updateStLit (xy,True) pcNow _ m =
  M.insert pcNow new m where
  new = case M.lookup pcNow m of
    Nothing ->      S.singleton $ LedBecauseSwitch xy
    Just reasons -> S.insert (LedBecauseSwitch xy) reasons

-- | When a key is released, it might be that we've used the arrows
-- since pressing it. If that's so, then the pitch it triggered is elsewhere,
-- and illuminated. This removes the pitch from the LitPitches,
-- so that the appropriate pitch class will be darkened.
updateStLit (xy,False) _ mpcThen m =
  case mpcThen of
    Nothing -> m
    Just pc ->
      -- TODO (#safety) Check that that's really what's being deleted.
      let Just reasons = M.lookup pc m
      in case S.size reasons < 2 of -- size < 1 should not happen
        True -> M.delete pc m
        False -> M.insert pc (S.delete (LedBecauseSwitch xy) reasons) m
