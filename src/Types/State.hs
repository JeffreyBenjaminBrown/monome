module Types.State where

import Data.Map as M
import Data.Set as S
import Vivid

import Math31 (Pitch, PitchClass)
import Synth
import Types.Button
import Util.Byte
import Util.Network


data State = State {
    inbox :: Socket
  , toMonome :: Socket -- ^ PITFALL : some function arguments share this name
  , voices :: M.Map (X,Y) (Synth BoopParams)
  , xyShift :: (X,Y) -- ^ this is relative -- a vector, not a point
  , fingers :: M.Map (X,Y) PitchClass
  , lit :: M.Map PitchClass (S.Set LedReason)
  , sustainOn :: Bool
  , sustained :: S.Set (X,Y)
  } deriving (Show, Eq)
