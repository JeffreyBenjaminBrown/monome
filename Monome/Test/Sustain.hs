{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Monome.Test.Sustain where

import Test.HUnit

import           Control.Lens
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)

import Monome.Math31
import Monome.Test.Data
import Monome.Types.Button
import Monome.Types.Initial
import Monome.Window.Keyboard as K
import Monome.Window.Sustain  as Su


tests :: Test
tests = TestList [
    TestLabel "test_sustainHandler" test_sustainHandler
  , TestLabel "test_deleteOneSustainedNote_and_insertOneSustainedNote"
    test_deleteOneSustainedNote_and_insertOneSustainedNote
  , TestLabel "test_toggleSustain" test_toggleSustain
  , TestLabel "test_voicesToSilence_uponSustainOff" test_voicesToSilence_uponSustainOff
  ]

test_voicesToSilence_uponSustainOff :: Test
test_voicesToSilence_uponSustainOff = TestCase $ do
  assertBool "Turn off sustain. Voice 0 is fingered, 1 is turned off." $
    voicesToSilence_uponSustainOff st_0fs_1s == S.singleton v1

test_toggleSustain :: Test
test_toggleSustain = TestCase $ do
  assertBool "turn sustain on" $
    toggleSustain st_0f =^=
    ( st_0f & ( stLit . at pc0 . _Just %~ S.insert LedBecauseSustain )
      & stSustained .~ Just (S.singleton v0 ) )
  assertBool "turn sustain off" $
    toggleSustain st_0s
    =^= ( st_0s & stLit .~ mempty
          & stSustained .~ Nothing )
  assertBool "turn sustain off, but finger persists" $
    toggleSustain st_0fs
    =^= ( st_0fs & stLit . at pc0 . _Just %~ S.delete LedBecauseSustain
          & stSustained .~ Nothing )

test_deleteOneSustainedNote_and_insertOneSustainedNote :: Test
test_deleteOneSustainedNote_and_insertOneSustainedNote = TestCase $ do
  let pc = 0
      lit_a :: Map PitchClass (Set LedBecause) = -- lit b/c anchor
        M.singleton pc $ S.singleton LedBecauseAnchor
      lit_s :: Map PitchClass (Set LedBecause) = -- lit b/c/ sustain
        M.singleton pc $ S.singleton LedBecauseSustain
      lit_as :: Map PitchClass (Set LedBecause) = -- lit b/c both
        M.singleton pc $ S.fromList [LedBecauseAnchor, LedBecauseSustain]
  assertBool "add sustain to the reasons a lit (because anchored) key is lit"
    $ insertOneSustainedNote pc lit_a == lit_as
  assertBool "add sustain to the previously empty set of reasons a key is lit"
    $ insertOneSustainedNote pc mempty == lit_s

  assertBool "if sustain was the only reason, then upon releasing sustain, there are no more reasons" $
    deleteOneSustainedNote pc lit_s == mempty
  assertBool "if the anchor note is sustained, then upon releasing sustain, the anchor reemains as reason to light the key" $
    deleteOneSustainedNote pc lit_as == lit_a

test_sustainHandler :: Test
test_sustainHandler = TestCase $ do
  assertBool "releasing (not turning off) the sustain button has no effect"
    $ Su.handler st0 (meh , False) =^= st0

  assertBool "turning ON sustain changes the sustain state, the set of sustained voices, the set of reasons for keys to be lit, and the messages pending to the monome." $
    Su.handler st_0f (meh, True)
    =^= st_0f { _stSustained = Just $ S.singleton v0
              , _stLit = M.singleton pc0 $
                         S.fromList [ LedBecauseSustain
                                    , LedBecauseSwitch xy0 ]
              , _stPending_Monome =
                [ (Su.label, (Su.theButton, True)) ] }

  let st_0fs_1s_sustainOff = st_0fs_1s
        & stSustained .~ mempty
        & stLit .~ M.singleton pc0 ( S.singleton $
                                     LedBecauseSwitch xy0 )
        & stPending_Monome .~
        ( ( Su.label, (Su.theButton, False)) :
          map (\xy -> (K.label, (xy, False)))
          (pcToXys (_stXyShift st_0fs_1s) pc1) )
        & stPending_Vivid .~ [ SoundMsg $ ParamMsg
                               { _paramMsgVoiceId = v1
                               , _paramMsgPitch = Nothing
                               , _paramMsgVal = 0
                               , _paramMsgParam = "amp" }
                             , SoundMsgFree v1 ]
    in do
    assertBool "turning sustain OFF remove all `LedBecauseSustain`s from reasons for lit keys" $
      _stLit (Su.handler st_0fs_1s (meh, True))
      == _stLit st_0fs_1s_sustainOff
    assertBool "turning sustain OFF adds messages for the monome to turn off the sustain button and the keys that were sustained and are not fingered" $
      _stLit (Su.handler st_0fs_1s (meh, True))
      == _stLit st_0fs_1s_sustainOff
    assertBool "turning sustain OFF adds messages for Vivid to turn off any pitches from voices that were sustained and are not fingered" $
      _stPending_Vivid (Su.handler st_0fs_1s (meh, True))
      == _stPending_Vivid st_0fs_1s_sustainOff
    assertBool "turning sustain OFF (any remaining fields)" $
      Su.handler st_0fs_1s (meh, True)
      =^= st_0fs_1s_sustainOff

