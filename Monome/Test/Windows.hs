{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Monome.Test.Windows where

import Test.HUnit

import qualified Data.Map as M
import qualified Data.Set as S

import Monome.Math31
import Monome.Types.Button
import Monome.Types.Initial
import Monome.Window.Keyboard as K
import Monome.Window.Shift    as Sh
import Monome.Window.Sustain  as Su


tests :: Test
tests = TestList [
    TestLabel "test_shiftHandler" test_shiftHandler
  , TestLabel "test_sustainHandler" test_sustainHandler
  ]

meh :: a
meh = error "not relevant to this test"

st0 :: St
st0 = St {
    _stVoices = mempty
  , _stPending_Monome = []
  , _stPending_Vivid = []
  , _stXyShift = (0,0)
  , _stFingers = mempty
  , _stLit = mempty
  , _stSustainOn = False
  , _stSustained = mempty
  }

(=^=) :: St -> St -> Bool
(=^=) x y = and [
    _stPending_Monome x == _stPending_Monome y
  , _stPending_Vivid x  == _stPending_Vivid y
  , _stXyShift x        == _stXyShift y
  , _stFingers x        == _stFingers y
  , _stLit x            == _stLit y
  , _stSustainOn x      == _stSustainOn y
  , _stSustained x      == _stSustained y]

test_shiftHandler :: Test
test_shiftHandler = TestCase $ do
  let st = st0 { _stLit =
                 M.singleton 0 $ S.singleton LedBecauseAnchor }
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

test_sustainHandler :: Test
test_sustainHandler = TestCase $ do
  let fingerAt = (0,0)
      voiceId = (0,0)
      pitchClass = 0
      st1 = st0 { _stFingers =
                  M.singleton fingerAt (voiceId, pitchClass) }
  assertBool "releasing (not turning off) the sustain button has no effect"
    $ Su.handler st0 (meh , False) =^= st0
  assertBool "turning on sustain changes the sustain state, the set of sustained voices, the set of reasons for keys to be lit, and the messages pending to the monome." $
    Su.handler st1 (meh, True)
    =^= st1 { _stSustainOn = True
            , _stSustained =
              S.singleton (voiceId, pitchClass)
            , _stLit =
              M.singleton pitchClass $ S.singleton LedBecauseSustain
            , _stPending_Monome =
              [ (Su.label, (Su.theButton, True)) ] }
  assertBool "TODO: test turning on sustain" False
