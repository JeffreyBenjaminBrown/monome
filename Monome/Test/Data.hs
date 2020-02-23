{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monome.Test.Data where

import           Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

import Monome.Types.Initial
import Monome.Math31


(=^=) :: St -> St -> Bool
(=^=) x y = and [
    _stPending_Monome x == _stPending_Monome y
  , _stPending_Vivid x  == _stPending_Vivid y
  , _stXyShift x        == _stXyShift y
  , _stFingers x        == _stFingers y
  , _stLit x            == _stLit y
  , _stSustained x      == _stSustained y]

v0     :: VoiceId    = (0,0)
v1     :: VoiceId    = (0,1)
xy0    :: (X,Y)      = v0
xy1    :: (X,Y)      = v1
pitch0 :: Pitch      = xyToEt31_st st0 xy0
pitch1 :: Pitch      = xyToEt31_st st0 xy1
pc0    :: PitchClass = mod pitch0 31
pc1    :: PitchClass = mod pitch1 31

st0 :: St
st0 = St {
    _stVoices = mempty
  , _stPending_Monome = []
  , _stPending_Vivid = []
  , _stXyShift = (3,5)
  , _stFingers = mempty
  , _stLit = mempty
  , _stSustained = Nothing
  }

st_0a = -- 0 is the anchor pitch
  st0 & stLit %~ M.insert pc0 (S.singleton LedBecauseAnchor)

st_0f = -- fingering key 0 only
  st0 & stFingers .~ M.fromList [ ( xy0, ( v0, pc0) ) ]
  & stLit .~  M.fromList
  [ ( pc0, S.singleton $ LedBecauseSwitch xy0) ]

st_0s = -- sustaining key 0 only
  st0
  & stLit .~  M.singleton pc0
  (S.singleton LedBecauseSustain)
  & stSustained .~ Just (S.singleton (v0, pc0) )

st_01f = -- fingering keys 0 and 1
  st0 & stFingers .~ M.fromList [ ( xy0, ( v0, pc0) )
                                , ( xy1, ( v1, pc1) ) ]
  & stLit .~ M.fromList
  [ ( pc0, S.singleton $ LedBecauseSwitch xy0)
  , ( pc1, S.singleton $ LedBecauseSwitch xy1) ]

st_0fs = -- 0 is both fingered and sustained
  st_0f
  & stSustained .~ Just (S.singleton (v0, pc0) )
  & stLit .~  ( M.singleton pc0
                $ S.fromList [ LedBecauseSwitch xy0
                             , LedBecauseSustain ] )

st_0af = -- 0 is both fingered and the anchor pitch
  st_0f & stLit . at pc0 . _Just
  %~ S.insert LedBecauseAnchor