{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Monome.Window.Shift (
    handler
  , label
  , shiftWindow

  , shift
  , leftArrow, rightArrow, upArrow, downArrow, upOctave, downOctave -- ^ (X,Y)
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import qualified Data.Map as M

import           Monome.Math31
import           Monome.Types.Button
import           Monome.Types.Initial
import           Monome.Util
import qualified Monome.Window.Keyboard as Kbd


label :: WindowId
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
-- | PITFALL: There are multiple ways to represent an octave shift.
-- Here I've chosen one arbitrarily.
shift :: (X,Y) -> (X,Y)
shift xy | xy == rightArrow = ( 1, 0)
         | xy == downArrow  = ( 0, 1)
           -- origin at top-left => down means add to Y
         | xy == leftArrow  = (-1, 0)
         | xy == upOctave   = (-5,-1)
           -- lowering the origin raises the coordinate values of a given key, hence raising its pitch
         | xy == upArrow    = ( 0,-1)
         | xy == downOctave = ( 5, 1)
         | otherwise = error $ "shift: unexpected input: " ++ show xy

-- | = the window
shiftWindow :: Window EtApp
shiftWindow = Window {
    windowLabel = label
  , windowContains = \(x,y) -> numBetween 13 15 x && numBetween 14 15 y
  , windowInit = stPending_Monome %~ flip (++)
    ( (label,) . (,True) <$>
      [ upArrow, downArrow, leftArrow, rightArrow ] )
  , windowRoutine = handler
}

handler :: St EtApp -> ((X,Y), Switch) -> St EtApp
handler    st0         (_,  False)      = st0
handler    st0         (xy, True )      = let
  st' :: St EtApp = st0 & stApp . etXyShift %~ addPair (shift xy)
  lit :: [PitchClass] = M.keys $ st0 ^. stApp . etLit
  msgs :: [LedMsg] =
    map (Kbd.label,) $
    (map (,False) $ concatMap (pcToXys $ st0 ^. stApp . etXyShift) lit) ++
    (map (,True)  $ concatMap (pcToXys $ st' ^. stApp . etXyShift) lit)
  in st' & stPending_Monome %~ flip (++) msgs
