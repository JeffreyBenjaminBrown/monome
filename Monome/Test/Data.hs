{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Monome.Test.Data where

import Monome.Types.Initial


(=^=) :: St -> St -> Bool
(=^=) x y = and [
    _stPending_Monome x == _stPending_Monome y
  , _stPending_Vivid x  == _stPending_Vivid y
  , _stXyShift x        == _stXyShift y
  , _stFingers x        == _stFingers y
  , _stLit x            == _stLit y
  , _stSustained x      == _stSustained y]

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
