{-# LANGUAGE TupleSections #-}

module Window.Shift (
  shiftWindow
  , colorAnchors
  , colorArrows
  , label
  ) where

import Control.Concurrent.MVar
import Data.List as L
import Data.NumInstances

import Math31
import Types.Window
import Types.Button
import Types.State
import Util.Byte
import Util.Network
import Window.Common (colorAnchors)
import qualified Window.Keyboard


label = "shift window"

shiftWindow = Window {
  windowLabel = label
  , windowContains = \(x,y) -> numBetween x 0 1 && numBetween y 13 15
  , windowInit = \_ toShiftWindow -> colorArrows toShiftWindow
  , windowHandler = handler
}

colorArrows :: LedRelay -> IO ()
colorArrows toShiftWindow = mapM_ f [ (0,15),(0,14),(0,13)
                               , (1,14) ]
  where f = toShiftWindow . (,LedOn)

handler :: MVar State -> LedRelay -> [Window] -> ((X,Y), Switch) -> IO ()
handler    _             _           _           (_,  SwitchOff) = return ()
handler    mst           toShift     ws          (xy, SwitchOn ) = do
  st <- takeMVar mst
  let Just keyboard = L.find pred ws where
                      pred = (==) Window.Keyboard.label . windowLabel
      toKeyboard = colorIfHere (toMonome st) ws keyboard
      shift :: (X,Y) -> (PitchClass, (X,Y))
      shift xy = case xy of (0,15) -> ( 6, ( 0, 1)) 
                            (0,14) -> ( 1, (-1, 0))
                            (0,13) -> (-6, ( 1, 0))
                            (1,15) -> ( 0, ( 1, -5)) -- up octave
                            (1,14) -> (-1, ( 1,  0))
                            (1,13) -> ( 0, (-1, 5))  -- down octave
      (anchorShift, xyShiftShift) = shift xy
      newAnchor = anchor st + anchorShift
  colorAnchors toKeyboard (anchor st) LedOff
  colorAnchors toKeyboard newAnchor LedOn
  putMVar mst $ st { xyShift = xyShift st + xyShiftShift
                   , anchor = mod newAnchor 31 }
