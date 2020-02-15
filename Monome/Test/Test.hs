module Monome.Test.Test (tests) where

import Data.Map as M
import Data.Set as S
import Test.HUnit

import Monome.Window.Common
import Monome.Types.Button
import Monome.Types.Window


tests :: IO Counts
tests = runTestTT $ TestList [
    TestLabel "testBelongsHere" testBelongsHere
  , TestLabel "testDependentPitchClass" testDependentPitchClass
  ]

testDependentPitchClass :: Test
testDependentPitchClass = TestCase $ do
  let m = M.singleton 10 $ S.singleton $ LedBecauseSwitch (1,1)
  assertBool "ledBecause_toPitchClass finds it" $
    ledBecause_toPitchClass m (LedBecauseSwitch (1,1)) == Just 10
  assertBool "ledBecause_toPitchClass does not find it" $
    ledBecause_toPitchClass m  (LedBecauseSwitch (1,0)) == Nothing

testBelongsHere :: Test
testBelongsHere = TestCase $ do
  let nmr = NoMVarRoutine $ \st _ _ _ -> return st
      w1 = Window "w1" (\(x,y) -> x > y) mempty nmr
      w2 = Window "w2" (\(x,_) -> x > 4) mempty nmr
      w3 = Window "w3" (\(_,_) -> True)  mempty nmr
      ws = [w1,w2,w3]
  assertBool "caught by w1 before reaching w3" $
    not $ belongsHere ws w3 ((1,0),True)
  assertBool "caught by w2 before reaching w3" $
    not $ belongsHere ws w3 ((5,6),True)
  assertBool "should reach w3, which contains it" $
    belongsHere ws w3 ((0,0),True)
  assertBool "should reach w2, but w2 does not contain it" $
    not $ belongsHere ws w2 ((1,2),True)
