{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
, TupleSections
, ScopedTypeVariables
#-}

module Monome.Window.JI (
    handler
  , jiWindow
  , label

  , jiFreq -- ^ JiApp -> (X,Y) -> Either String Float
  ) where

import           Prelude hiding (pred)
import           Control.Lens

import Monome.Types.Button
import Monome.Types.Initial
import Monome.Util


label :: WindowId
label = "ji window"

jiWindow :: Window JiApp
jiWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInit = id
  , windowRoutine = handler }

handler :: St JiApp
        -> ((X,Y), Switch)
        -> St JiApp
handler st press @ (xy,sw) =
  error "like the Keyboard handler"

jiKeySound :: St JiApp -> ((X,Y), Switch) -> [SoundMsg]
jiKeySound st (xy,switch) =
  error "like keyMsg"

jiFreq :: JiApp -> (X,Y) -> Either String Float
jiFreq ja (x,y) = do
  let (octave :: Int, rowInOctave :: Int) =
        divMod y $ length $ ja ^. jiShifts
  f0 :: Float <- let
    err = Left $ "key x-value " ++ show x ++ " but generator only has (0-indexed) length " ++ show (length $ ja ^. jiGenerator)
    in maybe err Right $ ja ^? jiGenerator . ix x
  Right $ f0 * ((ja ^. jiShifts) !! rowInOctave) * 2 ** fi octave
    -- !! is safe here, because of the divMod above
