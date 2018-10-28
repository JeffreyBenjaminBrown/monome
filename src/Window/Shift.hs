{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Window.Shift (
  shiftWindow
  , colorArrows
  , label
  ) where

import Control.Concurrent.MVar
import qualified Data.List as L
import qualified Data.Map as M

import Math31
import Types.Window
import Types.Button
import Types.State
import Util.Byte
import Util.Network
import Window.Common (drawPitchClass)
import qualified Window.Keyboard


label = "shift window"

-- | = the arrows
rightArrow = (15,15)
downArrow =  (14,15)
leftArrow =  (13,15)
upOctave =   (15,14)
upArrow =    (14,14)
downOctave = (13,14)

shift :: (X,Y) -> (X,Y)
shift xy | xy == rightArrow = ( 1, 0)
         | xy == downArrow  = ( 0, 1)
         | xy == leftArrow  = (-1, 0)
         | xy == upOctave   = (-5,-1)
         | xy == upArrow    = ( 0,-1)
         | xy == downOctave = ( 5, 1)

-- | = the window
shiftWindow = Window {
  windowLabel = label
  , windowContains = \(x,y) -> numBetween 13 15 x && numBetween 14 15 y
  , windowInit = \_ toShiftWindow -> colorArrows toShiftWindow
  , windowHandler = handler
}

colorArrows :: LedRelay -> IO ()
colorArrows toShiftWindow = let f = toShiftWindow . (,LedOn)
  in mapM_ f [ upArrow, downArrow, leftArrow, rightArrow ]

handler :: MVar State -> LedRelay -> [Window] -> ((X,Y), Switch) -> IO ()
handler    _             _           _           (_,  SwitchOff) = return ()
handler    mst           toShift     ws          (xy, SwitchOn ) = do
  st <- takeMVar mst
  let Just keyboard = L.find pred ws where -- unsafe but it must be in there
        pred = (==) Window.Keyboard.label . windowLabel
      toKeyboard = relayIfHere (toMonome st) ws keyboard
      st' = st { xyShift = addPair (xyShift st) (shift xy) }
      draw st = drawPitchClass toKeyboard $ xyShift st
  mapM_ (draw st  LedOff) $ M.keys $ lit st
  mapM_ (draw st' LedOn ) $ M.keys $ lit st'
  putMVar mst st'
