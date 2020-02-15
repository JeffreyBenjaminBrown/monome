{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Monome.Window.Shift (
    shiftWindow
  , colorArrows
  , label
  ) where

import Prelude hiding (pred)
import Control.Concurrent.MVar
import qualified Data.Map as M

import           Monome.Types.Window
import           Monome.Types.Button
import           Monome.Types.State
import           Monome.Util
import           Monome.Window.Common (drawPitchClass)
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

-- | PITFALL: Remember (see Button.hs),
-- higher Y => lower (closer to you) on the monome.
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
  , windowRoutine = NoMVarRoutine handler
}

colorArrows :: LedRelay -> IO ()
colorArrows toShiftWindow = let f = toShiftWindow . (,True)
  in mapM_ f [ upArrow, downArrow, leftArrow, rightArrow ]

handler :: St -> LedRelay -> [Window] -> ((X,Y), Switch) -> IO (St)
handler    st0   _           _           (_,  False) = return st0
handler    st0   _           ws          (xy, True ) = do
  let st' = st0 { stXyShift = addPair (stXyShift st0) (shift xy) }
      draw st = drawPitchClass toKeyboard $ stXyShift st
        where toKeyboard = relayToWindow st0 Kbd.label ws
  mapM_ (draw st0 False) $ M.keys $ stLit st0
  mapM_ (draw st' True ) $ M.keys $ stLit st'
  return st'
