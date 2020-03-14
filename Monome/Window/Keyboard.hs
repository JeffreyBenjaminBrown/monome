{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
, ScopedTypeVariables
, TupleSections
, TypeApplications
#-}

module Monome.Window.Keyboard (
    handler
  , keyboardWindow
  , label
  ) where

import           Prelude hiding (pred)
import           Control.Lens
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

keyboardWindow :: Window EtApp
keyboardWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInit = \st ->
      st & stPending_Monome %~
      flip (++) ( map ( (label,) . (,True) ) $
                  concatMap (pcToXys $ st ^. stApp . etXyShift) $
                  M.keys $ st ^. stApp . etLit )
  , windowRoutine = handler }

handler :: St EtApp -> ((X,Y), Switch) -> St EtApp
handler    st          press@ (xy,sw)   = let
  fingers' = st ^. stApp . etFingers
        & case sw of
            True  -> M.insert xy xy
            False -> M.delete xy
  pcNow :: (PitchClassRep EtApp) =
    mod (xyToEt31_st st xy) 31
    -- what the key represents currently
  pcThen :: Maybe (PitchClassRep EtApp) =
    ledBecause_toPitchClass @ EtApp
    (st ^. stApp . etLit) $ LedBecauseSwitch xy
    -- what the key represented when it was pressed,
    -- if it is now being released
  lit  :: LitPitches EtApp = st ^. stApp . etLit
  lit' :: LitPitches EtApp = updateStLit (xy,sw) pcNow pcThen lit
  oldKeys :: Set (PitchClassRep EtApp) = S.fromList $ M.keys $ lit
  newKeys :: Set (PitchClassRep EtApp) = S.fromList $ M.keys $ lit'
  toDark  :: [PitchClassRep EtApp] = S.toList $ S.difference oldKeys newKeys
  toLight :: [PitchClassRep EtApp] = S.toList $ S.difference newKeys oldKeys
  kbdMsgs :: [LedMsg] =
    map (label,) $
    ( map (,False) $
      concatMap (pcToXys $ st ^. stApp . etXyShift) toDark) ++
    ( map (,True)  $
      concatMap (pcToXys $ st ^. stApp . etXyShift) toLight)
  soundMsgs :: [SoundMsg EtApp] = etKey_SoundMsg st press
  st1 :: St EtApp = st
    & stApp . etFingers .~ fingers'
    & stApp . etLit     .~ lit'
    & stPending_Monome  %~ (++ kbdMsgs)
    & stPending_Vivid   %~ (++ soundMsgs)
  in foldr updateVoice st1 soundMsgs

updateStLit :: ((X,Y), Switch)
  -> PitchClassRep EtApp         -- ^ what xy represents now
  -> Maybe (PitchClassRep EtApp) -- ^ what xy represented when last pressed
  -> LitPitches EtApp
  -> LitPitches EtApp

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
