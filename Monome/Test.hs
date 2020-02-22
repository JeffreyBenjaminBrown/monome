module Monome.Test (tests) where

import Test.HUnit

import qualified Monome.Test.Misc
import qualified Monome.Test.Windows


tests :: IO Counts
tests = runTestTT $ TestList
  [ Monome.Test.Misc.tests
  , Monome.Test.Windows.tests
  ]
