{-# LANGUAGE TupleSections #-}

module Window.Common where

import Math31
import Types.Button
import Types.Window


colorAnchors :: LedRelay -> PitchClass -> Led -> IO ()
colorAnchors toKeyboardWindow anchor led = mapM_ f xys
  where xys = enharmonicToXYs $ et31ToLowXY anchor
        f = toKeyboardWindow . (,led)
