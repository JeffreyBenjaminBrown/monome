module Monome.Test where

import Test.HUnit

import qualified Monome.Test.Misc
import qualified Monome.Test.Sustain
import qualified Monome.Test.Windows
import qualified Monome.Test.JI

doTests :: IO Counts
doTests = runTestTT $ TestList
  [ Monome.Test.Misc.tests
  , Monome.Test.Windows.tests
  , Monome.Test.Sustain.tests
  , Monome.Test.JI.tests
  ]
