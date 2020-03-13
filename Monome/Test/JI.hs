{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Monome.Test.JI where

import Test.HUnit

import           Data.Either

import Monome.Types.Initial
import Monome.Window.JI


tests :: Test
tests = TestList [
    TestLabel "test_jiFreq" test_jiFreq
  ]

test_jiFreq :: Test
test_jiFreq = TestCase $ do
  let ja = JiApp { _jiFingers = error "meh"
                 , _jiShifts = [1,3/2]
                 , _jiGenerator = [1,5/4] }
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
