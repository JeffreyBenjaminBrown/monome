{-# LANGUAGE TemplateHaskell #-}

module Monome.Types.Initial (
    HostName, Socket
  , WindowId, VoiceId
  , Pitch, PitchClass, LitPitches
  , LedMsg, SoundMsg
  , X, Y, Switch, Led
  , LedBecause(..)
  , Window(..)
  , St(..), stWindowLayers, stToMonome, stVoices, stPending_Monome
    , stPending_Vivid, stXyShift, stFingers, stLit, stSustainOn, stSustained
  ) where

import           Control.Lens
import           Data.Map
import           Data.Set
import qualified Network.Socket as NS
import           Vivid hiding (Param)

import Monome.Synth.Boop


type HostName = NS.HostName
type Socket = NS.Socket

type Param = String
type WindowId = String
type VoiceId = (Int,Int) -- ^ I would call this (X,Y),
                         -- but it would require a cyclic dependency.

-- | Pitch is isomorphic to the integers.
-- PitchClass is isomorphic to the integers modulo 31.
-- That is, PitchClass 0 is identical to PitchClass 31,
-- whereas Pitch 31 is an octave above Pitch 0.
-- Pitch classes are (so far) only relevant in the visual context:
-- if a key is lit up on the keyboard, so are all enharmonic notes,
-- in all octaves.
type Pitch      = Int
type PitchClass = Int
type LitPitches = Map PitchClass (Set LedBecause)
  -- ^ For each pitch class that is lit,
  -- we need to know why -- e.g. if it's being sustained,
  -- then we should not darken it when the finger on it is lifted,
  -- and if it's an anchor, we should never darken it.
  -- The Set is a Set because an LED could be on for multiple reasons.

type LedMsg   = (WindowId, ((X,Y), Led))
type SoundMsg = ( VoiceId
                , Maybe Pitch -- ^ messages like "turn off" don't need one
                , Float
                , Param )

-- | X and Y are coordinates on the monome.
-- PITFALL: X rises from left to right, but Y rises from top to bottom.
-- Thus (0,1) is just under the top-left corner.
-- PITFALL: The monome will respond to out-of-bounds (x,y) values.
-- I don't use that feature.
type X = Int
type Y = Int

type Switch = Bool -- | Whether a monome button is pressed.
type Led    = Bool -- | Whether a monome LED is lit.

-- | The reason a (pitch class of) LED(s) in the keyboard window is lit.
data LedBecause =
    LedBecauseSwitch (X,Y)
  | LedBecauseSustain
  | LedBecauseAnchor -- ^ Some "visual anchor" pitches are always on.
  deriving (Show, Eq, Ord)

data Window = Window {
    windowLabel :: WindowId -- ^ PITFALL: Must be unique across windows,
    -- or the Eq instance fails.
  , windowContains :: (X,Y) -> Bool
    -- ^ PITFALL: A monome will respond to out-of-bounds (x,y) values.
    -- Every Window therefore needs a nontrivial windowContains field,
    -- even the background Window.
  , windowInit :: St -> St
  , windowRoutine :: -- ^ Acts on messages from the monome.
      St
      -> ((X,Y), Switch) -- ^ the incoming button press|release
      -> St
  }

instance Eq Window where
  (==) a b = windowLabel a == windowLabel b

instance Show Window where
  show = windowLabel

data St = St {
    _stWindowLayers :: [Window] -- ^ PITFALL: Order matters.
      -- Key presses are handled by the first window containing them.
      -- Windows listed earlier are thus "above" later ones.
  , _stToMonome :: Socket
  , _stVoices :: Map VoiceId (Synth BoopParams, Pitch, Map String Float)
    -- ^ TODO ? This is expensive, precluding the use of big synths.
    -- Maybe I could make them dynamically without much speed penalty.
    -- Tom of Vivid thinks so.

  -- | The purpose of `_stPending_Monome` and `_stPending_Vivid`
  -- is to isolate side-effects to a small portion of the code. Elsewhere,
  -- scattered functions can simply change an `St` instead of doing IO.
  , _stPending_Monome :: [LedMsg]
  , _stPending_Vivid :: [SoundMsg]

  , _stXyShift :: (X,Y) -- ^ this is relative -- a vector, not a point
  , _stFingers :: Map (X,Y) PitchClass
    -- ^ Where each finger is, and what it's lighting up.
    -- Note that this doesn't track what pitch it started.
    -- Since voices are indexed by (X,Y), that's okay.
  , _stLit :: LitPitches

  , _stSustainOn :: Bool
    -- ^ TODO ? This could be eliminated by making the next field a Maybe.
  , _stSustained :: Set (VoiceId, PitchClass)
    -- ^ PITFALL: In spirit, the thing sustained is a Pitch,
    -- but it's represented as a voice,
    -- identified by the key that originally launched it.
  } deriving (Show, Eq)
makeLenses ''St
