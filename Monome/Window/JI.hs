{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
, TupleSections
, ScopedTypeVariables
#-}

module Monome.Window.JI (
    handler
  , jiWindow
  , label
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Set (Set)

import Monome.Math31
import Monome.Types.Button
import Monome.Types.Initial
import Monome.Util
import Monome.Window.Common


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
