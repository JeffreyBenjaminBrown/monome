{-# LANGUAGE TupleSections #-}

module Window.Shift (
  shiftWindow
  , colorAnchors
  , colorArrows
  ) where

import Control.Concurrent.MVar
import Data.List as L

import ET31.Keyboard
import Types.App
import Types.Button
import Util.Byte
import Util.Network


shiftWindow = Window {
  windowLabel = "shiftWindow"
  , windowContains = \(x,y) -> numBetween x 0 1 && numBetween y 13 15
  , windowInit = \_ toShiftWindow -> colorArrows toShiftWindow
  , windowHandler = handler
}

colorAnchors :: LedRelay -> Int -> Led -> IO ()
colorAnchors toKeyboardWindow anchor led = mapM_ f xys
  where xys = enharmonicToXYs $ et31ToLowXY anchor
        f = toKeyboardWindow . (,led)

colorArrows :: LedRelay -> IO ()
colorArrows toShiftWindow = mapM_ f [ (0,15),(0,14),(0,13)
                               , (1,14) ]
  where f = toShiftWindow . (,LedOn)

handler :: MVar State -> LedRelay -> [Window] -> ((X,Y), Switch) -> IO ()
handler    _             _           _           (_,  SwitchOff) = return ()
handler    mst           toShift     ws          (xy, SwitchOn ) = do
  st <- takeMVar mst
  let Just keyboard = L.find ((==) "keyboardWindow" . windowLabel) ws
      toKeyboard = colorIfHere (toMonome st) ws keyboard
      anchorShift = case xy of (0,15) -> 6
                               (0,14) -> 1
                               (1,14) -> -1
                               (0,13) -> -6
                               _ -> 0
      pitchShift = case xy of (0,15) -> -6
                              (1,15) -> 31
                              (0,14) -> -1
                              (1,14) -> 1
                              (0,13) -> 6
                              (1,13) -> -31
      newAnchor = anchor st + anchorShift
  colorAnchors toKeyboard (anchor st) LedOff
  colorAnchors toKeyboard newAnchor LedOn
  putMVar mst $ st { shift = shift st + pitchShift
                   , anchor = mod newAnchor 31 }
