{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
, TupleSections
, ScopedTypeVariables
#-}

module Monome.Window.Keyboard (
    keyboardWindow
  , label
  ) where

import Prelude hiding (pred)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Set (Set)

import Monome.Math31
import Monome.Types.Button
import Monome.Types.Initial
import Monome.Util
import Monome.Window.Common


label :: WindowId
label = "keyboard window"

keyboardWindow :: Window
keyboardWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInit = \st ->
      st { _stPending_Monome =
            map ( (label,) . (,True) ) $
            concatMap (pcToXys $ _stXyShift st) $
            M.keys $ _stLit st }
  , windowRoutine = handler }

handler :: St
        -> ((X,Y), Switch)
        -> St
handler st press @ (xy,sw) = do
  let pcNow :: PitchClass =
        mod (xyToEt31 $ addPair xy $ negPair $ _stXyShift st) 31
        -- what the key represents currently
      pcThen :: Maybe PitchClass =
        ledBecause_toPitchClass (_stLit st) $ LedBecauseSwitch xy
        -- what the key represented when it was pressed,
        -- if it is now being released
      fingers' = case sw of
        True  -> M.insert xy pcNow $ _stFingers st
        False -> M.delete xy $ _stFingers st
      lit' :: LitPitches = updateStLit (xy,sw) pcNow pcThen $ _stLit st
      oldKeys :: Set PitchClass  = S.fromList $ M.keys $ _stLit st
      newKeys :: Set PitchClass  = S.fromList $ M.keys $ lit'
      toDark  ::    [PitchClass] = S.toList $ S.difference oldKeys newKeys
      toLight ::    [PitchClass] = S.toList $ S.difference newKeys oldKeys
      kbdMsgs :: [LedMsg] =
        map (label,) $
        (map (,False) $ concatMap (pcToXys $ _stXyShift st) toDark) ++
        (map (,True)  $ concatMap (pcToXys $ _stXyShift st) toLight)
  st { _stFingers = fingers'
     , _stPending_Monome = kbdMsgs ++ _stPending_Monome st
     , _stPending_Vivid = soundKeySt st press ++ _stPending_Vivid st
     , _stLit = lit' }

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
      -- todo ? (#safety) Check that that's really what's being deleted.
      let Just reasons = M.lookup pc m
      in case S.size reasons < 2 of -- size < 1 should not happen
        True -> M.delete pc m
        False -> M.insert pc (S.delete (LedBecauseSwitch xy) reasons) m
