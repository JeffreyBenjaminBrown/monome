module Monome.Test where

import Test.HUnit

import qualified Monome.Test.Misc
import qualified Monome.Test.Windows


doTests :: IO Counts
doTests = runTestTT $ TestList
  [ Monome.Test.Misc.tests
  , Monome.Test.Windows.tests
  ]
