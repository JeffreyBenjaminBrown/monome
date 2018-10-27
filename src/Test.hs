module Test (tests) where

import Test.HUnit

tests = runTestTT $ TestList [
    TestLabel "testTest" testTest
    ]

testTest = TestCase $ do
  assertBool "hunit works" $ 4 > 3
