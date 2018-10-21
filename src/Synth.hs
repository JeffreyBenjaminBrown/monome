{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Synth where

import Vivid


type BoopParams = '["freq","amp"]

boop :: SynthDef BoopParams
boop = sd ( 0 :: I "freq"
          , 0 :: I "amp"
          ) $ do
  sin <- tanh' $ sinOsc (freq_ (V::V "freq"))
  s1 <- (V::V "amp") ~* sin ~* sin
  out 0 [s1, s1]
