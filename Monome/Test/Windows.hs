{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Monome.Test.Windows where

import Test.HUnit

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Monome.Math31
import Monome.Types.Button
import Monome.Types.Initial
import Monome.Types.Window
import Monome.Window.Common
import Monome.Window.Keyboard as K
import Monome.Window.Shift    as Sh
import Monome.Window.Sustain  as Su


tests :: Test
tests = TestList [
    TestLabel "test_shiftHandler" test_shiftHandler
  ]

test_shiftHandler :: Test
test_shiftHandler = TestCase $ do
  let meh = error "not relevant to this test"
      (=^=) :: St -> St -> Bool
      (=^=) x y =
        _stPending_Monome x == _stPending_Monome y &&
        _stXyShift x        == _stXyShift y &&
        _stLit x            == _stLit y
      st = St {
          _stPending_Monome = []
        , _stXyShift = (0,0)
        , _stLit = M.singleton 0 $ S.singleton LedBecauseAnchor
        }
  assertBool "releasing a shift button does nothing" $
    Sh.handler st (meh, False) =^= st
  assertBool "shift the notes one space closer to player's body" $
    let nudge :: (X,Y) = Sh.shift Sh.downArrow
        msgs :: [LedMsg] = map (K.label,)
                           $  map (,False) (pcToXys (0,0) 0)
                           ++ map (,True)  (pcToXys nudge 0)
    in Sh.handler st (Sh.downArrow, True) =^=
       st { _stPending_Monome = msgs
          , _stXyShift = nudge }
  assertBool "shift the notes an octave higher" $
    let nudge :: (X,Y) = Sh.shift Sh.upOctave
        msgs :: [LedMsg] = map (K.label,)
          $  map (,False) (pcToXys (0,0) 0)
          ++ map (,True)  (pcToXys nudge 0)
    in Sh.handler st (Sh.upOctave, True) =^=
       st { _stPending_Monome = msgs
          , _stXyShift = nudge }
