module Test (tests) where

import Test.HUnit

import Types.App
import Types.Button


tests = runTestTT $ TestList [
    TestLabel "testBelongsHere" testBelongsHere
    ]

testBelongsHere = TestCase $ do
  let emptyHandler = \_ _ _ _ -> return ()
      w1 = Window "w1" (\(x,y) -> x > y) emptyHandler
      w2 = Window "w2" (\(x,y) -> x > 4) emptyHandler
      w3 = Window "w3" (\(x,y) -> True)  emptyHandler
      ws = [w1,w2,w3]
  assertBool "caught by w1 before reaching w3" $
    not $ belongsHere ws w3 ((1,0),LedOn)
  assertBool "caught by w2 before reaching w3" $
    not $ belongsHere ws w3 ((5,6),LedOn)
  assertBool "should reach w3, which contains it" $
    belongsHere ws w3 ((0,0),LedOn)
  assertBool "should reach w2, but w2 does not contain it" $
    not $ belongsHere ws w2 ((1,2),LedOn)
