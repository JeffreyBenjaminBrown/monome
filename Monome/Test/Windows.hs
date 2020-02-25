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


tests :: Test
tests = TestList [
    TestLabel "test_shiftHandler" test_shiftHandler
  , TestLabel "test_keyboardHandler" test_keyboardHandler
  ]

test_shiftHandler :: Test
test_shiftHandler = TestCase $ do
  assertBool "releasing a shift button does nothing" $
    Sh.handler (meh, False) st_0a =^= st_0a

  assertBool "shift the notes one space closer to player's body" $ let
    oldShift = _stXyShift st_0a
    newShift = addPair oldShift $ Sh.shift Sh.downArrow
    msgs :: [LedMsg] = map (K.label,)
      $  map (,False) (pcToXys oldShift pc0)
      ++ map (,True)  (pcToXys newShift pc0)
    in Sh.handler (Sh.downArrow, True) st_0a
    =^= st_0a { _stPending_Monome = msgs
              , _stXyShift = newShift }

  assertBool "shift the notes an octave higher" $ let
    oldShift = _stXyShift st_0a
    newShift = addPair oldShift $ Sh.shift Sh.upOctave
    msgs :: [LedMsg] = map (K.label,)
      $  map (,False) (pcToXys oldShift pc0)
      ++ map (,True)  (pcToXys newShift pc0)
    in Sh.handler (Sh.upOctave, True) st_0a =^=
       st_0a { _stPending_Monome = msgs
             , _stXyShift = newShift }

test_keyboardHandler :: Test
test_keyboardHandler = TestCase $ do
  let st_01f_r1 = -- st_0f after releasing key 1
        st_0f
        & ( stPending_Monome .~
            map (\xy -> (K.label, (xy, False)) )
            (pcToXys (_stXyShift st_01f) pitch1 ) )
        & stPending_Vivid .~ [ SoundMsg $ ParamMsg
                               { _paramMsgVoiceId = v1
                               , _paramMsgPitch = Nothing
                               , _paramMsgVal = 0
                               , _paramMsgParam = "amp" }
                             , SoundMsgFree v1 ]
    in do
    assertBool "releasing a key sends off-messages to monome" $
      _stPending_Monome (K.handler (xy1, False) st_01f)
      == _stPending_Monome st_01f_r1
    assertBool "releasing a key sends off-messages to Vivid" $
      _stPending_Vivid (K.handler (xy1, False) st_01f)
      == _stPending_Vivid st_01f_r1
    assertBool "releasing a key removes something from _stFingers" $
      _stFingers (K.handler (xy1, False) st_01f)
      == _stFingers st_01f_r1
    assertBool "releasing a key removes somehing from _stLit" $
      _stLit (K.handler (xy1, False) st_01f)
      == _stLit st_01f_r1
    assertBool "releasing a key (all the above tests combined)" $
      K.handler (xy1, False) st_01f =^= st_01f_r1

  assertBool "releasing a key that's also the anchor pitch sends no monome messages" $
    K.handler (xy0, False) st_0af
    =^= ( st_0af
          & ( stLit . at pc0 . _Just
              .~ S.singleton LedBecauseAnchor )
          & stFingers .~ mempty
          & stPending_Vivid .~ [ SoundMsg $ ParamMsg
                                 { _paramMsgVoiceId = v0
                                 , _paramMsgPitch = Nothing
                                 , _paramMsgVal = 0
                                 , _paramMsgParam = "amp" }
                               , SoundMsgFree v0 ] )

  assertBool "releasing a key that's a sustained voice sends no vivid or monome messages, but updates lit and fingers" $
    K.handler (xy0, False) st_0fs
    =^= ( st_0fs
          & ( stLit . at pc0 . _Just
              .~ S.singleton LedBecauseSustain )
          & stFingers .~ mempty )

  assertBool "pressing a key that's a sustained voice updates stFingers and stLit" $
    K.handler (xy0, True) st_0s
    =^= ( st_0s & ( stLit . at pc0 . _Just
                    %~ S.insert (LedBecauseSwitch xy0) )
          & stFingers .~ M.fromList [ (xy0,v0) ] )

  assertBool "pressing a key sends on-messages to monome, sends on-messages to Vivid, adds something to _stFingers, and asdds something from _stLit" $
    K.handler (xy1, True) st_0f
    =^= ( st_01f
          & ( stPending_Monome .~
              map (\xy -> (K.label, (xy, True)) )
              (pcToXys (_stXyShift st_01f) pitch1 ) )
          & stPending_Vivid .~ keyMsg st0_2voices_35shift (xy1,True) )
