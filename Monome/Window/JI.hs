{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
, TupleSections
, ScopedTypeVariables
#-}

module Monome.Window.JI (
    handler
  , jiWindow
  , label

  , jiFreq -- ^ JiApp -> (X,Y) -> Either String Float
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import           Data.Either
import qualified Data.Set as S

import qualified Monome.Config as Config
import Monome.Types.Button
import Monome.Types.Initial
import Monome.Util
import Monome.Window.Common


label :: WindowId
label = "ji window"

jiWindow :: Window JiApp
jiWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInit = id
  , windowRoutine = handler }

handler :: St JiApp
        -> ((X,Y), Switch)
        -> St JiApp
handler st press @ (xy,sw) =
  error "like the keyboard handler"

-- TODO ! duplicative of `etKey_SoundMsg`
-- TODO ! untested
jiKey_SoundMsg :: JiApp -> ((X,Y), Switch) -> [SoundMsg]
jiKey_SoundMsg ja (xy,switch) = let
  doIfKeyFound :: Float -> [SoundMsg]
  doIfKeyFound freq =
    if switch
      then let msg = SoundMsg
                     { _soundMsgVoiceId = xy
                     , _soundMsgPitch = Just $ floor freq } -- HACK. `_soundMsgPitch` Isn't used in the JI app, but it is copied to somewhere else, so I can't leave the field unset.
           in [ msg & soundMsgVal .~ Config.freq * freq
                & soundMsgParam .~ "freq"
              , msg & soundMsgVal .~ Config.amp
                & soundMsgParam .~ "amp" ]
      else [silenceMsg xy]
  in either (const []) doIfKeyFound $ jiFreq ja xy
     -- [] if key out of range; key corresponds to no pitch

jiFreq :: JiApp -> (X,Y) -> Either String Float
jiFreq ja (x,y) = do
  let (octave :: Int, rowInOctave :: Int) =
        divMod y $ length $ ja ^. jiShifts
  f0 :: Float <- let
    err = Left $ "key x-value " ++ show x ++ " but generator only has (0-indexed) length " ++ show (length $ ja ^. jiGenerator)
    in maybe err Right $ ja ^? jiGenerator . ix x
  Right $ f0 * ((ja ^. jiShifts) !! rowInOctave) * 2 ** fi octave
    -- !! is safe here, because of the divMod above
