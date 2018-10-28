{-# LANGUAGE TupleSections #-}

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

rightArrow = (15,15)
downArrow =  (14,15)
leftArrow =  (13,15)
upOctave =   (15,14)
upArrow =    (14,14)
downOctave = (13,14)

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
  let Just keyboard = L.find pred ws where
                      pred = (==) Window.Keyboard.label . windowLabel
      toKeyboard = colorIfHere (toMonome st) ws keyboard
      shift :: (X,Y) -> (PitchClass, (X,Y))
      shift xy | xy == rightArrow = ( 6, ( 1, 0))
               | xy == downArrow  = ( 1, ( 0, 1))
               | xy == leftArrow  = (-6, (-1, 0))
               | xy == upOctave   = ( 0, (-5,-1))
               | xy == upArrow    = (-1, ( 0,-1))
               | xy == downOctave = ( 0, ( 5, 1))
      (anchorShift, xyShiftShift) = shift xy
      drawPitchClass' = drawPitchClass toKeyboard $ xyShift st
  mapM_ (drawPitchClass' LedOff                 ) $ M.keys $ lit st
  mapM_ (drawPitchClass' LedOn . (+) anchorShift) $ M.keys $ lit st
  putMVar mst $ st { xyShift = addPair (xyShift st) xyShiftShift }
