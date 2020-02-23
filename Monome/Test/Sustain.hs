{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Monome.Test.Sustain where

import Test.HUnit

import           Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

import Monome.Math31
import Monome.Test.Data
import Monome.Types.Button
import Monome.Types.Initial
import Monome.Window.Keyboard as K
import Monome.Window.Sustain  as Su


tests :: Test
tests = TestList [
    TestLabel "test_sustainHandler" test_sustainHandler
  ]

test_sustainHandler :: Test
test_sustainHandler = TestCase $ do
  assertBool "releasing (not turning off) the sustain button has no effect"
    $ Su.handler st0 (meh , False) =^= st0

  assertBool "turning ON sustain changes the sustain state, the set of sustained voices, the set of reasons for keys to be lit, and the messages pending to the monome." $
    Su.handler st_0f (meh, True)
    =^= st_0f { _stSustained = Just $ S.singleton v0
              , _stLit = M.singleton pc0 $
                         S.fromList [ LedBecauseSustain
                                    , LedBecauseSwitch xy0 ]
              , _stPending_Monome =
                [ (Su.label, (Su.theButton, True)) ] }

  assertBool ( "turning sustain OFF does all this stuff:\n" ++
               "flip the sustain state\n" ++
               "emptiy the set of sustained voices\n" ++
               "remove all `LedBecauseSustain`s from reasons for lit keys\n"
               ++ "adds messages for the monome to turn off the sustain button and the keys that were sustained and are not fingered\n" ++
               " adds messages for Vivid to turn off any pitches from voices that were sustained and are not fingered\n" ++
               "Pitch 0 is fingered, and 0 and 1 sounding; 1 turns off.") $
    let st_0f' = st_0f
          & stLit .~
          M.fromList [ (0, S.fromList [ LedBecauseSustain
                                      , LedBecauseSwitch xy0 ] )
                     , (1, S.fromList [ LedBecauseSustain ] ) ]
          & stSustained .~ Just ( S.fromList [ (0,0)
                                             , (0,1) ] )
    in Su.handler st_0f' (meh, True)
       =^=  ( st_0f'
              & stSustained .~ mempty
              & stLit .~ M.singleton 0 (S.singleton $
                                        LedBecauseSwitch xy0 )
              & stPending_Monome .~
              ( ( Su.label, (Su.theButton, False)) :
                map (\xy -> (K.label, (xy, False)))
                (pcToXys (_stXyShift st_0f') 1) )
              & stPending_Vivid .~ [ SoundMsg { _soundMsgVoiceId = (0,1)
                                              , _soundMsgPitch = Nothing
                                              , _soundMsgVal = 0
                                              , _soundMsgParam = "amp" } ] )
