module Types.State where

import Data.Map as M
import Data.Set as S
import Vivid

import Synth
import Types.Button
import Util.Byte
import Util.Network


data State = State {
    inbox :: Socket
  , toMonome :: Socket -- ^ PITFALL : some function arguments share this name
  , voices :: M.Map (X,Y) (Synth BoopParams)
  , anchor :: Int
  , shift :: Float -- ^ multiplicative; 2 = one octave higher
  , fingers :: S.Set (X,Y)
  , sustainOn :: Bool
  , sustained :: S.Set (X,Y)
  } deriving (Show, Eq)
