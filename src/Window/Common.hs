{-# LANGUAGE TupleSections #-}

module Window.Common where

import Data.NumInstances

import Math31
import Types.Button
import Types.Window


colorAnchors :: LedRelay -> PitchClass -> (X,Y) -> Led -> IO ()
colorAnchors toKeyboardWindow anchor xyShift led = mapM_ f xys
  where xys = enharmonicToXYs $ et31ToLowXY $ anchor + xyToEt31 xyShift
        f = toKeyboardWindow . (,led)
