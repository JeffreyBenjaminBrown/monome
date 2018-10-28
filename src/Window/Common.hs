{-# LANGUAGE TupleSections #-}

module Window.Common where

import Math31
import Types.Button
import Types.Window
import Util.Byte


colorAnchors :: LedRelay
             -> PitchClass -- ^ what to light up
             -> (X,Y)      -- ^ a shift
             -> Led -> IO ()
colorAnchors toKeyboardWindow pitchClass xyShift led = mapM_ f xys
  where xys = enharmonicToXYs $ addPair (et31ToLowXY pitchClass) xyShift
        f = toKeyboardWindow . (,led)
