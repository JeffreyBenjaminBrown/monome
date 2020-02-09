module Monome.Types.State where

import Data.Map as M
import Data.Set as S
import Vivid

import Monome.Math31
import Monome.Synth.Boop
import Monome.Types.Button
import Monome.Network.Util


data State = State {
    stInbox :: Socket
  , stToMonome :: Socket
  , stVoices :: M.Map (X,Y) (Synth BoopParams)
  , stXyShift :: (X,Y) -- ^ this is relative -- a vector, not a point
  , stFingers :: M.Map (X,Y) PitchClass
  , stLit :: M.Map PitchClass (S.Set LedReason)
  , stSustainOn :: Bool
  , stSustained :: S.Set ((X,Y), PitchClass)
    -- ^ PITFALL: In spirit, the thing sustained is a PitchClass,
    -- but it's represented as a voice,
    -- identified by the key that originally launched it.
  } deriving (Show, Eq)
