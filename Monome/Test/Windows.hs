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
    Sh.handler st_0a (meh, False) =^= st_0a

  assertBool "shift the notes one space closer to player's body" $ let
    oldShift = st_0a ^. stApp . etXyShift
    newShift = addPair oldShift $ Sh.shift Sh.downArrow
    msgs :: [LedMsg] = map (K.label,)
      $  map (,False) (pcToXys oldShift pc0)
      ++ map (,True)  (pcToXys newShift pc0)
    in Sh.handler st_0a (Sh.downArrow, True)
    =^= (st_0a & stPending_Monome .~ msgs
               & stApp . etXyShift .~ newShift)

  assertBool "shift the notes an octave higher" $ let
    oldShift = st_0a ^. stApp . etXyShift
    newShift = addPair oldShift $ Sh.shift Sh.upOctave
    msgs :: [LedMsg] = map (K.label,)
      $  map (,False) (pcToXys oldShift pc0)
      ++ map (,True)  (pcToXys newShift pc0)
    in Sh.handler st_0a (Sh.upOctave, True) =^=
       (st_0a & stPending_Monome .~ msgs
              & stApp . etXyShift .~ newShift)

test_keyboardHandler :: Test
test_keyboardHandler = TestCase $ do
  assertBool "releasing a key sends off-messages to monome, sends off-messages to Vivid, removes something from _etFingers, and removes some things from _etLit" $
    K.handler st_01f (xy1, False)
    =^= ( st_0f
          & ( stPending_Monome .~
              map (\xy -> (K.label, (xy, False)) )
              (pcToXys (st_01f ^. stApp . etXyShift) pitch1 ) )
          & stPending_Vivid .~ [SoundMsg { _soundMsgVoiceId = v1
                                         , _soundMsgPitch = Nothing
                                         , _soundMsgVal = 0
                                         , _soundMsgParam = "amp" } ] )

  assertBool "releasing a key that's also the anchor pitch sends no monome messages" $
    K.handler st_0af (xy0, False)
    =^= ( st_0af
          & ( stApp . etLit . at pc0 . _Just
              .~ S.singleton LedBecauseAnchor )
          & stApp . etFingers .~ mempty
          & stPending_Vivid .~ [SoundMsg { _soundMsgVoiceId = v0
                                         , _soundMsgPitch = Nothing
                                         , _soundMsgVal = 0
                                         , _soundMsgParam = "amp" } ] )

  assertBool "releasing a key that's a sustained voice sends no vivid or monome messages, but updates lit and fingers" $
    K.handler st_0fs (xy0, False)
    =^= ( st_0fs
          & ( stApp . etLit . at pc0 . _Just
              .~ S.singleton LedBecauseSustain )
          & stApp . etFingers .~ mempty )

  assertBool "pressing a key that's a sustained voice updates etFingers and etLit" $
    K.handler st_0s (xy0, True)
    =^= ( st_0s & ( stApp . etLit . at pc0 . _Just
                    %~ S.insert (LedBecauseSwitch xy0) )
          & stApp . etFingers .~ M.fromList [ (xy0,v0) ] )

  assertBool "pressing a key sends on-messages to monome, sends on-messages to Vivid, adds something to _etFingers, and asdds something from _etLit" $
    K.handler st_0f (xy1, True)
    =^= ( st_01f
          & ( stPending_Monome .~
              map (\xy -> (K.label, (xy, True)) )
              (pcToXys (st_01f ^. stApp . etXyShift) pitch1 ) )
          & stPending_Vivid .~ etKeyMsg st0 (xy1,True) )
