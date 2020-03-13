{-# LANGUAGE TemplateHaskell #-}

module Monome.Types.Initial (
    HostName, Socket
  , Param, WindowId, VoiceId
  , Pitch, PitchClass, LitPitches
  , LedMsg
  , SoundMsg(..), soundMsgVoiceId, soundMsgPitch, soundMsgVal, soundMsgParam
  , X, Y, Switch, Led
  , LedBecause(..)
  , Window(..)
  , Voice(..), voiceSynth, voicePitch, voiceParams
  , St(..), stApp, stWindowLayers, stToMonome, stVoices
    , stPending_Monome, stPending_Vivid
  , EtApp(..), etXyShift, etFingers, etLit, etSustaineded
  , JiApp(..), jiGenerator, jiShifts, jiFingers
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
data SoundMsg = SoundMsg {
    _soundMsgVoiceId :: VoiceId
  , _soundMsgPitch   :: Maybe Pitch -- ^ messages like "off" don't need one
  , _soundMsgVal     :: Float
  , _soundMsgParam   :: Param }
  deriving (Show, Eq, Ord)

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

data Window app = Window {
    windowLabel :: WindowId -- ^ PITFALL: Must be unique across windows,
    -- or the Eq instance fails.
  , windowContains :: (X,Y) -> Bool
    -- ^ PITFALL: A monome will respond to out-of-bounds (x,y) values.
    -- Every Window therefore needs a nontrivial windowContains field,
    -- even the background Window.
  , windowInit :: St app -> St app
  , windowRoutine :: -- ^ Acts on messages from the monome.
      St app
      -> ((X,Y), Switch) -- ^ the incoming button press|release
      -> St app
  }

instance Eq (Window  app) where
  (==) a b = windowLabel a == windowLabel b

instance Show (Window app) where
  show = windowLabel

data Voice = Voice {
    _voiceSynth  :: Synth BoopParams
  , _voicePitch  :: Pitch -- ^ PITFALL: This looks redundant, but isn't.
    -- The frequency is indeed stored in the _voiceParams field,
    -- but that's a float; it's nice to have the integer Pitch around too.
  , _voiceParams :: Map String Float }
  deriving (Show, Eq, Ord)

data St app = St {
    _stApp :: app
  , _stWindowLayers :: [Window  app] -- ^ PITFALL: Order matters.
      -- Key presses are handled by the first window containing them.
      -- Windows listed earlier are thus "above" later ones.
  , _stToMonome :: Socket -- ^ PITFALL: It's tempting to remove this from St.
    -- That's feasible now, ll want it here when using multiple monomes.
  , _stVoices :: Map VoiceId Voice
    -- ^ TODO ? This is expensive, precluding the use of big synths.
    -- Maybe I could make them dynamically without much speed penalty.
    -- Tom of Vivid thinks so.

  -- | The purpose of `_stPending_Monome` and `_stPending_Vivid`
  -- is to isolate side-effects to a small portion of the code. Elsewhere,
  -- scattered functions can simply change an `St` instead of doing IO.
  , _stPending_Monome :: [LedMsg]
  , _stPending_Vivid :: [SoundMsg]
  } deriving (Show, Eq)

data EtApp = EtApp
  { _etXyShift :: (X,Y) -- ^ this is relative -- a vector, not a point
  , _etFingers :: Map (X,Y) VoiceId
    -- ^ Where fingers are, what each is sounding,
    -- and what each is lighting up.
  , _etLit :: LitPitches
  , _etSustaineded :: Maybe (Set VoiceId)
    -- ^ PITFALL: In spirit, the thing sustained is a Pitch,
    -- but it's represented as a voice,
    -- identified by the key that originally launched it.
  } deriving (Show, Eq)

data JiApp = JiApp
  { _jiGenerator :: [Float]
  , _jiShifts :: [Float]
  , _jiFingers :: Map (X,Y) VoiceId }
  deriving (Show, Eq)

makeLenses ''SoundMsg
makeLenses ''Voice
makeLenses ''St
makeLenses ''EtApp
makeLenses ''JiApp
