{-# LANGUAGE TupleSections #-}

module Monome.Window.Common where

import Prelude hiding (pred)
import qualified Data.Map as M
import qualified Data.Maybe as Mb
import qualified Data.Set as S

import Monome.Math31
import Monome.Types.Button
import Monome.Types.Window
import Monome.Util


drawPitchClass :: LedRelay
               -> (X,Y)      -- ^ a shift
               -> Led
               -> PitchClass -- ^ what to light up
               -> IO ()
drawPitchClass toKeyboardWindow xyShift led pitchClass = do
  let xys = enharmonicToXYs $ addPair (et31ToLowXY pitchClass) xyShift
      f = toKeyboardWindow . (,led)
  mapM_ f xys

-- TODO (#speed) Instead, keep a map from xy to pitchclass
dependentPitchClass :: M.Map PitchClass (S.Set LedReason)
                    -> LedReason -> Maybe PitchClass
dependentPitchClass m ldr = fst <$> mPair
  where pred = S.member ldr . snd
        mPair = Mb.listToMaybe $ filter pred $ M.toList m
  -- if I'm right, laziness => `fst` makes this stop at the first result