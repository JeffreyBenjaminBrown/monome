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
import Vivid

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
      mapM_ (drawPitchClass toKeyboard (stXyShift st) LedOn)
        $ M.keys $ stLit st
  , windowRoutine = handler }

handler :: MVar State
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
        -- a pitch the key lit up in the past
      fingers' = case sw of
        SwitchOn -> M.insert xy pcNow $ stFingers st
        SwitchOff -> M.delete xy $ stFingers st
      nl = updateStLit (xy,sw) pcNow pcThen $ stLit st
      oldKeys = S.fromList $ M.keys $ stLit st
      newKeys = S.fromList $ M.keys $ nl
      toDark = S.difference oldKeys newKeys
      toLight = S.difference newKeys oldKeys

  -- putStrLn . show $ fingers' -- handy spot to print
  mapM_ (drawPitchClass toKeyboard (stXyShift st) LedOff) toDark
  mapM_ (drawPitchClass toKeyboard (stXyShift st) LedOn) toLight
  putMVar mst $ st { stFingers = fingers'
                   , stLit = nl }

soundKey :: State -> ((X,Y), Switch) -> IO ()
soundKey st (xy, sw) = do
  let pitchClass = xyToEt31 xy - xyToEt31 (stXyShift st)
  case S.member xy $ S.map fst $ stSustained st of
    True -> return () -- it's already sounding due to sustain
    False ->
      let freq = 100 * et31ToFreq pitchClass
          voice = (M.!) (stVoices st) xy
      in set voice ( toI freq                    :: I "freq"
                   , toI $ 0.15 * switchToInt sw :: I "amp" )


updateStLit :: ((X,Y), Switch)
       -> PitchClass       -- ^ what xy represents now
       -> Maybe PitchClass -- ^ what xy represented when it was last pressed
       -> LitPitches
       -> LitPitches

-- | When a button is newly pressed,
-- it adds anoother LedBecause to the LitPitches.
updateStLit (xy,SwitchOn) pcNow _ m =
  M.insert pcNow new m where
  new = case M.lookup pcNow m of
    Nothing ->      S.singleton $ LedBecauseSwitch xy
    Just reasons -> S.insert (LedBecauseSwitch xy) reasons

-- | When a key is released, it might be that we've used the arrows
-- since pressing it. If that's so, then the pitch it triggered is elsewhere,
-- and illuminated. This removes the pitch from the LitPitches,
-- so that the appropriate pitch class will be darkened.
updateStLit (xy,SwitchOff) _ mpcThen m =
  case mpcThen of
    Nothing -> m
    Just pc ->
      -- TODO (#safety) Check that that's really what's being deleted.
      let Just reasons = M.lookup pc m
      in case S.size reasons < 2 of -- size < 1 should not happen
        True -> M.delete pc m
        False -> M.insert pc (S.delete (LedBecauseSwitch xy) reasons) m
