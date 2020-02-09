{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Monome.Window.Shift (
  shiftWindow
  , colorArrows
  , label
  ) where

import Prelude hiding (pred)
import Control.Concurrent.MVar
import qualified Data.List as L
import qualified Data.Map as M

import Monome.Types.Window
import Monome.Types.Button
import Monome.Types.State
import Monome.Util
import Monome.Window.Common (drawPitchClass)
import qualified Monome.Window.Keyboard as Kbd


label :: WindowLabel
label = "shift window"

-- | = the arrows
rightArrow, downArrow, leftArrow, upOctave, upArrow, downOctave :: (X,Y)
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
         | otherwise = error $ "shift: unexpected input: " ++ show xy

-- | = the window
shiftWindow :: Window
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
handler    mst           _           ws          (xy, SwitchOn ) = do
  st0 <- takeMVar mst
  let Just keyboard = L.find pred ws where -- unsafe but it must be in there
        pred = (==) Kbd.label . windowLabel
      toKeyboard = relayIfHere (stToMonome st0) ws keyboard
      st' = st0 { stXyShift = addPair (stXyShift st0) (shift xy) }
      draw st = drawPitchClass toKeyboard $ stXyShift st
  mapM_ (draw st0 LedOff) $ M.keys $ stLit st0
  mapM_ (draw st' LedOn ) $ M.keys $ stLit st'
  putMVar mst st'