{-# LANGUAGE TupleSections #-}

module Window.Common where

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
