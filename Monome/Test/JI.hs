{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Monome.Test.JI where

import Test.HUnit

import           Control.Lens
import           Data.Either

import qualified Monome.Config as Config
import           Monome.Types.Initial
import           Monome.Window.JI


tests :: Test
tests = TestList [
    TestLabel "test_jiFreq" test_jiFreq
  , TestLabel "test_jiKeySound" test_jiKeySound
  ]

ja :: JiApp
ja = JiApp { _jiFingers = error "meh"
           , _jiShifts = [1,3/2]
           , _jiGenerator = [1,5/4] }

test_jiKeySound :: Test
test_jiKeySound = TestCase $ do
  let f :: (X,Y) -> IO ()
      f xy = let
        Right freq = jiFreq ja xy
        msg = SoundMsg { _soundMsgVoiceId = xy
                       , _soundMsgPitch = Just $ floor freq }
        in do
        assertBool "sound on" $ jiKey_SoundMsg ja (xy,True)
          == [ msg & soundMsgVal .~ Config.freq * freq
               & soundMsgParam .~ "freq"
             , msg & soundMsgVal .~ Config.amp
               & soundMsgParam .~ "amp" ]
        assertBool "sound off" $ jiKey_SoundMsg ja (xy,False)
          == [ msg & soundMsgPitch .~ Nothing
               & soundMsgVal .~ 0
               & soundMsgParam .~ "amp" ]
        assertBool "out of x-range" $
          jiKey_SoundMsg ja (xy & _1 +~ 100, False) == []
  mapM_ f [(0,0), (1,1), (1,3)]

test_jiFreq :: Test
test_jiFreq = TestCase $ do
  assertBool "x-direction can be out of range" $
    isLeft (jiFreq ja (10,0))
  assertBool "y-direction cannot be out of range" $
    isRight (jiFreq ja (0,1000))

  assertBool "unit" $
    jiFreq ja (0,0) == Right 1
  assertBool "the other pitch in the generator" $
    jiFreq ja (1,0) == Right (5/4)
  assertBool "the other (non-unity) shift" $
    jiFreq ja (0,1) == Right (3/2)
  assertBool "shifted in both the generator and the shifts" $
    jiFreq ja (1,1) == Right (15/8)

  assertBool "octave" $
    jiFreq ja (0,2) == Right 2
  assertBool "octave + generator + shift" $
    jiFreq ja (1,3) == Right (15/4)
