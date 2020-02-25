{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Monome.Test.Misc where

import Data.Map as M
import Data.Set as S
import Test.HUnit

import qualified Monome.Config as Config
import           Monome.Math31
import           Monome.Test.Data
import           Monome.Types.Button
import           Monome.Types.Initial
import           Monome.Types.Window
import           Monome.Util
import           Monome.Window.Common


tests :: Test
tests = TestList [
    TestLabel "testBelongsHere" testBelongsHere
  , TestLabel "testDependentPitchClass" testDependentPitchClass
  , TestLabel "test_keyMsg" test_keyMsg
  , TestLabel "test_nextVoice" test_nextVoice
  ]

test_nextVoice :: Test
test_nextVoice = TestCase $ do
  assertBool "next of mempty is (0,0)" $
    nextVoice mempty == (0,0)
  assertBool "after (0,0) is (1,0)" $
    nextVoice (M.singleton (0,1) meh) == (1,0)

test_keyMsg :: Test
test_keyMsg = TestCase $ do
  let sustainedVoice :: VoiceId = (0,0)
      newVoice :: VoiceId = (0,1)
      st = st0 { _stSustained =
                 Just $ S.singleton sustainedVoice }
      newPitch = xyToEt31_st st newVoice
  assertBool "pressing a key that's sustained has no effect" $
    keyMsg st (sustainedVoice, True) == []
  assertBool "releasing a key that's sustained has no effect" $
    keyMsg st (sustainedVoice, False) == []
  assertBool "press a key that's not sustained" $
    keyMsg st (newVoice, True) ==
    [ SoundMsg $ ParamMsg { _paramMsgVoiceId = newVoice
                          , _paramMsgPitch = Just newPitch
                          , _paramMsgVal = 100 * et31ToFreq newPitch
                          , _paramMsgParam = "freq" }
    , SoundMsg $ ParamMsg  { _paramMsgVoiceId = newVoice
                           , _paramMsgPitch = Just newPitch
                           , _paramMsgVal = Config.voiceAmplitude
                           , _paramMsgParam = "amp" } ]
  assertBool "release a key that's not sustained" $
    keyMsg st (newVoice, False) ==
    [ SoundMsg $ ParamMsg  { _paramMsgVoiceId = newVoice
                           , _paramMsgPitch = Nothing
                           , _paramMsgVal = 0
                           , _paramMsgParam = "amp" } ]

testDependentPitchClass :: Test
testDependentPitchClass = TestCase $ do
  let m = M.singleton 10 $ S.singleton $ LedBecauseSwitch (1,1)
  assertBool "ledBecause_toPitchClass finds it" $
    ledBecause_toPitchClass m (LedBecauseSwitch (1,1)) == Just 10
  assertBool "ledBecause_toPitchClass does not find it" $
    ledBecause_toPitchClass m  (LedBecauseSwitch (1,0)) == Nothing

testBelongsHere :: Test
testBelongsHere = TestCase $ do
  let w1 = Window "w1" (\(x,y) -> x > y) id const
      w2 = Window "w2" (\(x,_) -> x > 4) id const
      w3 = Window "w3" (\(_,_) -> True)  id const
      ws = [w1,w2,w3]
  assertBool "caught by w1 before reaching w3" $
    not $ belongsHere ws w3 (1,0)
  assertBool "caught by w2 before reaching w3" $
    not $ belongsHere ws w3 (5,6)
  assertBool "should reach w3, which contains it" $
    belongsHere ws w3 (0,0)
  assertBool "should reach w2, but w2 does not contain it" $
    not $ belongsHere ws w2 (1,2)
