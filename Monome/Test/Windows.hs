module Monome.Test.Windows (tests) where

import Test.HUnit

import Data.Map as M
import Data.Set as S

import Monome.Types.Button
import Monome.Types.Initial
import Monome.Types.Window
import Monome.Window.Common
import Monome.Window.Keyboard
import Monome.Window.Shift
import Monome.Window.Sustain


tests :: Test
tests = TestList [
    TestLabel "test_sustainHandler" test_sustainHandler
  ]

test_sustainHandler :: Test
test_sustainHandler = TestCase $ do
  return ()
