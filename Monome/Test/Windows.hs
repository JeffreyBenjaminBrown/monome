{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Monome.Test.Windows where

import Test.HUnit

import           Control.Lens
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
  let st :: St = st0 { _stLit =
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
  let fingerAt :: (X,Y) = (0,0)
      soundingVoice :: VoiceId = (0,0)
      soundingPc :: PitchClass = 0
      st1 :: St = st0 & stFingers .~
        M.singleton fingerAt (soundingVoice, soundingPc)
        -- TODO : this should also have a lit key.
  assertBool "releasing (not turning off) the sustain button has no effect"
    $ Su.handler st0 (meh , False) =^= st0

  assertBool "turning ON sustain changes the sustain state, the set of sustained voices, the set of reasons for keys to be lit, and the messages pending to the monome." $
    Su.handler st1 (meh, True)
    =^= st1 { _stSustainOn = True
            , _stSustained =
              S.singleton (soundingVoice, soundingPc)
            , _stLit =
              M.singleton soundingPc $ S.singleton LedBecauseSustain
            , _stPending_Monome =
              [ (Su.label, (Su.theButton, True)) ] }

  assertBool ( "turning sustain OFF does all this stuff:\n" ++
               "flip the sustain state\n" ++
               "emptiy the set of sustained voices\n" ++
               "remove all `LedBecauseSustain`s from reasons for lit keys\n"
               ++ "adds messages for the monome to turn off the sustain button and the keys that were sustained and are not fingered\n" ++
               " adds messages for Vivid to turn off any pitches from voices that were sustained and are not fingered\n" ++
               "Pitch 0 is fingered, and 0 and 1 sounding; 1 turns off.") $
    let st1' = st1 & stSustainOn .~ True
          & stLit .~
          M.fromList [ (0, S.fromList [ LedBecauseSustain
                                      , LedBecauseSwitch fingerAt ] )
                     , (1, S.fromList [ LedBecauseSustain ] ) ]
          & stSustained .~ S.fromList [ ((0,0), 0)
                                      , ((0,1), 1) ]
    in Su.handler st1' (meh, True)
       =^=  ( st1' & stSustainOn .~ False
              & stSustained .~ mempty
              & stLit .~ M.singleton 0 (S.singleton $
                                        LedBecauseSwitch fingerAt )
              & stPending_Monome .~
              ( ( Su.label, (Su.theButton, False)) :
                map (\xy -> (K.label, (xy, False)))
                (pcToXys (_stXyShift st1') 1) )
              & stPending_Vivid .~ [ SoundMsg { _soundMsgVoiceId = (0,1)
                                              , _soundMsgPitch = Nothing
                                              , _soundMsgVal = 0
                                              , _soundMsgParam = "amp" } ] )
