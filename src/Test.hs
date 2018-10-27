module Test (tests) where

import Test.HUnit

import Types.App
import Types.Button


tests = runTestTT $ TestList [
    TestLabel "testBelongsHere" testBelongsHere
    ]

testBelongsHere = TestCase $ do
  let emptyHandler = const $ const $ return ()
      w1 = Window "w1" (\(x,y) -> x > y) emptyHandler
      w2 = Window "w2" (\(x,y) -> x > 4) emptyHandler
      w3 = Window "w3" (\(x,y) -> True)  emptyHandler
      ws = [w1,w2,w3]
  assertBool "?" $ not $ belongsHere ws w3 ((1,0),LedOn)
    -- caught by w1 before reaching w3
  assertBool "?" $ not $ belongsHere ws w3 ((5,6),LedOn)
    -- caught by w2 before reaching w3
  assertBool "?" $ belongsHere ws w3 ((0,0),LedOn)
    -- should reach w3, which contains it
  assertBool "?" $ not $ belongsHere ws w2 ((1,2),LedOn)
    -- should reach w2, but w2 does not contain it
