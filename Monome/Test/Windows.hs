{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Monome.Test.Windows where

import Test.HUnit

import           Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

import Monome.Math31
import Monome.Test.Data
import Monome.Types.Button
import Monome.Types.Initial
import Monome.Util
import Monome.Window.Common
import Monome.Window.Keyboard as K
import Monome.Window.Shift    as Sh
import Monome.Window.Sustain  as Su


tests :: Test
tests = TestList [
    TestLabel "test_shiftHandler" test_shiftHandler
  , TestLabel "test_sustainHandler" test_sustainHandler
  , TestLabel "test_keyboardHandler" test_keyboardHandler
  ]

meh :: a
meh = error "not relevant to this test"

test_shiftHandler :: Test
test_shiftHandler = TestCase $ do
  let st :: St = -- we need a lit `PitchClass` to move
        st0 & stLit .~ M.singleton 0 (S.singleton LedBecauseAnchor)

  assertBool "releasing a shift button does nothing" $
    Sh.handler st_0a (meh, False) =^= st_0a

  assertBool "shift the notes one space closer to player's body" $ let
    oldShift = _stXyShift st
    newShift = addPair oldShift $ Sh.shift Sh.downArrow
    msgs :: [LedMsg] = map (K.label,)
      $  map (,False) (pcToXys oldShift 0)
      ++ map (,True)  (pcToXys newShift 0)
    in Sh.handler st (Sh.downArrow, True)
    =^= st { _stPending_Monome = msgs
            , _stXyShift = newShift }

  assertBool "shift the notes an octave higher" $ let
    oldShift = _stXyShift st
    newShift = addPair oldShift $ Sh.shift Sh.upOctave
    msgs :: [LedMsg] = map (K.label,)
      $  map (,False) (pcToXys oldShift 0)
      ++ map (,True)  (pcToXys newShift 0)
    in Sh.handler st (Sh.upOctave, True) =^=
       st { _stPending_Monome = msgs
          , _stXyShift = newShift }

test_sustainHandler :: Test
test_sustainHandler = TestCase $ do
  assertBool "releasing (not turning off) the sustain button has no effect"
    $ Su.handler st0 (meh , False) =^= st0

  assertBool "turning ON sustain changes the sustain state, the set of sustained voices, the set of reasons for keys to be lit, and the messages pending to the monome." $
    Su.handler st_0f (meh, True)
    =^= st_0f { _stSustained = Just $
                             S.singleton (v0, pc0)
            , _stLit = M.singleton pc0 $
                       S.fromList [ LedBecauseSustain
                                  , LedBecauseSwitch xy0 ]
            , _stPending_Monome =
              [ (Su.label, (Su.theButton, True)) ] }

  assertBool ( "turning sustain OFF does all this stuff:\n" ++
               "flip the sustain state\n" ++
               "emptiy the set of sustained voices\n" ++
               "remove all `LedBecauseSustain`s from reasons for lit keys\n"
               ++ "adds messages for the monome to turn off the sustain button and the keys that were sustained and are not fingered\n" ++
               " adds messages for Vivid to turn off any pitches from voices that were sustained and are not fingered\n" ++
               "Pitch 0 is fingered, and 0 and 1 sounding; 1 turns off.") $
    let st_0f' = st_0f
          & stLit .~
          M.fromList [ (0, S.fromList [ LedBecauseSustain
                                      , LedBecauseSwitch xy0 ] )
                     , (1, S.fromList [ LedBecauseSustain ] ) ]
          & stSustained .~ Just ( S.fromList [ ((0,0), 0)
                                             , ((0,1), 1) ] )
    in Su.handler st_0f' (meh, True)
       =^=  ( st_0f'
              & stSustained .~ mempty
              & stLit .~ M.singleton 0 (S.singleton $
                                        LedBecauseSwitch xy0 )
              & stPending_Monome .~
              ( ( Su.label, (Su.theButton, False)) :
                map (\xy -> (K.label, (xy, False)))
                (pcToXys (_stXyShift st_0f') 1) )
              & stPending_Vivid .~ [ SoundMsg { _soundMsgVoiceId = (0,1)
                                              , _soundMsgPitch = Nothing
                                              , _soundMsgVal = 0
                                              , _soundMsgParam = "amp" } ] )

test_keyboardHandler :: Test
test_keyboardHandler = TestCase $ do
  assertBool "releasing a key sends off-messages to monome, sends off-messages to Vivid, removes something from _stFingers, and removes some things from _stLit" $
    K.handler st_01f (xy1, False)
    =^= ( st_0f
          & ( stPending_Monome .~
              map (\xy -> (K.label, (xy, False)) )
              (pcToXys (_stXyShift st_01f) pitch1 ) )
          & stPending_Vivid .~ [SoundMsg { _soundMsgVoiceId = v1
                                         , _soundMsgPitch = Nothing
                                         , _soundMsgVal = 0
                                         , _soundMsgParam = "amp" } ] )

  assertBool "releasing a key that's also the anchor pitch sends no monome messages" $
    K.handler st_0af (xy0, False)
    =^= ( st_0af
          & ( stLit . at (mod pitch0 31) . _Just
              .~ S.singleton LedBecauseAnchor )
          & stFingers .~ mempty
          & stPending_Vivid .~ [SoundMsg { _soundMsgVoiceId = v0
                                         , _soundMsgPitch = Nothing
                                         , _soundMsgVal = 0
                                         , _soundMsgParam = "amp" } ] )

  assertBool "releasing a key that's a sustained voice sends no vivid or monome messages, but updates lit and fingers" $
    K.handler st_0fs (xy0, False)
    =^= ( st_0fs
          & ( stLit . at (mod pitch0 31) . _Just
              .~ S.singleton LedBecauseSustain )
          & stFingers .~ mempty )

  assertBool "pressing a key that's a sustained voice updates stFingers and stLit" $
    K.handler st_0s (xy0, True)
    =^= ( st_0s & ( stLit . at (mod pitch0 31) . _Just
                    %~ S.insert (LedBecauseSwitch xy0) )
          & stFingers .~ M.fromList [ ( xy0, ( v0, mod pitch0 31) ) ] )

  assertBool "pressing a key sends on-messages to monome, sends on-messages to Vivid, adds something to _stFingers, and asdds something from _stLit" $
    K.handler st_0f (xy1, True)
    =^= ( st_01f
          & ( stPending_Monome .~
              map (\xy -> (K.label, (xy, True)) )
              (pcToXys (_stXyShift st_01f) pitch1 ) )
          & stPending_Vivid .~ keyMsg st0 (xy1,True) )
