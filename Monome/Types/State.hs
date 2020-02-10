module Monome.Types.State where

import Data.Map
import Data.Set
import Vivid

import Monome.Math31
import Monome.Synth.Boop
import Monome.Types.Button
import Monome.Network.Util


data St = St {
    stInbox :: Socket
  , stToMonome :: Socket
  , stVoices :: Map (X,Y) (Synth BoopParams)
    -- ^ TODO ? This is expensive, precluding the use of big synths.
    -- Maybe I could make them dynamically without much speed penalty.
    -- Tom of Vivid thinks so.
  , stXyShift :: (X,Y) -- ^ this is relative -- a vector, not a point
  , stFingers :: Map (X,Y) PitchClass
    -- ^ Where each finger is, and what it's lighting up.
    -- Note that this doesn't track what pitch it started.
    -- Since voices are indexed by (X,Y), that's okay.
  , stLit :: LitPitches
  , stSustainOn :: Bool
    -- ^ TODO ? This could be eliminated by making the next field a Maybe.
  , stSustained :: Set ((X,Y), PitchClass)
    -- ^ PITFALL: In spirit, the thing sustained is a PitchClass,
    -- but it's represented as a voice,
    -- identified by the key that originally launched it.
  } deriving (Show, Eq)
