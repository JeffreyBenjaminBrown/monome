{-# LANGUAGE TupleSections #-}

module Window.Common where

import qualified Data.Map as M
import qualified Data.Maybe as Mb
import qualified Data.Set as S

import Math31
import Types.Button
import Types.Window
import Types.State
import Util.Byte


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
                    -> (X,Y) -> Maybe PitchClass
dependentPitchClass m xy = fst <$> mPair
  where pred = S.member (LedFromSwitch xy) . snd
        mPair = Mb.listToMaybe $ filter pred $ M.toList m
  -- if I'm right, laziness => `fst` makes this stop at the first result
