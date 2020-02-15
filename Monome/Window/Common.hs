{-# LANGUAGE TupleSections #-}

module Monome.Window.Common where

import Prelude hiding (pred)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S

import Monome.Math31
import Monome.Types.Button
import Monome.Util


-- TODO : delete once unneeded
drawPitchClass :: LedRelay
               -> (X,Y)      -- ^ a shift
               -> Led
               -> PitchClass -- ^ what to light up
               -> IO ()
drawPitchClass toKeyboardWindow xyShift led pitchClass = do
  let xys = enharmonicToXYs $
            addPair (et31ToLowXY pitchClass) xyShift
      f = toKeyboardWindow . (,led)
  mapM_ f xys

-- TODO (#speed) Instead, keep a map from xy to pitchclass
ledBecause_toPitchClass ::
     LitPitches
  -> LedBecause
  -> Maybe PitchClass
ledBecause_toPitchClass m ldr =
  fst <$> mPair
  where
    mPair = listToMaybe
            $ filter (S.member ldr . snd)
            $ M.toList m
