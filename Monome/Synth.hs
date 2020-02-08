{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Monome.Synth (
  BoopParams
  , boop
  ) where

import Vivid


type BoopParams = '["freq","amp"]

-- | PITFALL: A default freq of 0 might seem natural,
-- but that causes a popping sounds when it's changed.
boop :: SynthDef BoopParams
boop = sd ( 100 :: I "freq"
          , 0 :: I "amp"
          ) $ do
  p <- pulse (freq_ (V::V "freq"))
  s <- saw (freq_ (V::V "freq"))
  s1 <- lag (in_ (V::V "amp"), lagSecs_ 0.03) ~* (p ~+ s ~/ 5)
    -- The lag smooths out discontinuities in the change in "amp".
  out 0 [s1, s1]
