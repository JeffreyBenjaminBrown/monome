{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Synth (
  BoopParams
  , boop
  ) where

import Vivid


type BoopParams = '["freq","amp"]

boop :: SynthDef BoopParams
boop = sd ( 0 :: I "freq"
          , 0 :: I "amp"
          ) $ do
  sin <- tanh' $ sinOsc (freq_ (V::V "freq"))
  s1 <- lag (in_ (V::V "amp"), lagSecs_ 0.01) ~* sin ~* sin
    -- The lag smooths out discontinuities in the change in "amp".
  out 0 [s1, s1]
