{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Monome.Window.Shift (
    shiftWindow
  , colorArrows
  , label
  ) where

import Prelude hiding (pred)
import qualified Data.Map as M

import           Monome.Math31
import           Monome.Types.Button
import           Monome.Types.State
import           Monome.Types.Window
import           Monome.Util
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

handler :: St -> ((X,Y), Switch) -> IO St
handler    st0   (_,  False)      = return st0
handler    st0   (xy, True )      = let
  st' = st0 { stXyShift = addPair (stXyShift st0) (shift xy) }
  lit  = M.keys $ stLit st0
  msgs :: [(WindowLabel, ((X,Y), Led))] =
    map (Kbd.label,) $
    (map (,False) $ concatMap (pcToXys $ stXyShift st0) lit) ++
    (map (,True)  $ concatMap (pcToXys $ stXyShift st') lit)
  in return st' { stPending_Monome = msgs }
