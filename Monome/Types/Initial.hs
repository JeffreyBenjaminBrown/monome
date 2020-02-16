module Monome.Types.Initial (
    HostName, Socket
  , WindowLabel, VoiceLabel
  , Pitch, PitchClass, LitPitches
  , X, Y, Switch, Led
  , LedBecause(..)
  , LedRelay
  , LedFilter
  , DeviceID(..)
  , Device(..)
  , St(..)
  , Window(..)
  , WindowRoutine
  ) where

import           Data.Map
import           Data.Set
import qualified Network.Socket as NS
import           Vivid

import Monome.Synth.Boop


type HostName = NS.HostName
type Socket = NS.Socket

type WindowLabel = String
type VoiceLabel = (Int,Int) -- ^ I would call this (X,Y),
                            -- but it would require a cyclic dependency.

-- | Pitch is isomorphic to the integers.
-- PitchClass is isomorphic to the integers modulo 31.
-- That is, PitchClass 0 is identical to PitchClass 31,
-- whereas Pitch 31 is an octave above Pitch 0.
type Pitch      = Int
type PitchClass = Int
type LitPitches = Map PitchClass (Set LedBecause)
  -- ^ For each pitch class that is lit,
  -- we need to know why -- e.g. if it's being sustained,
  -- then we should not darken it when the finger on it is lifted,
  -- and if it's an anchor, we should never darken it.
  -- The Set is a Set because an LED could be on for multiple reasons.

-- | PITFALL: X rises from left to right, but Y rises from top to bottom.
-- Thus (0,1) is just under the top-left corner.
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

-- | Forward a message to the monome if appropriate.
type LedRelay  = ((X,Y), Led) -> IO ()
type LedFilter = (X,Y) -> Bool

-- | SerialOsc responds to /serialosc/list messages with this information.
data DeviceID = DeviceID { deviceIDName :: ByteString
                         , deviceIDType :: ByteString
                         , deviceIDPort :: Int }
  deriving (Show, Eq, Ord)

-- | A monome (distinct form serialosc!) responds to /sys/info messages
-- with this information.
--
-- A device's DeviceInfo.deviceName = its DeviceID.deviceIDName.
-- That is the only pooint where `Device` and `DeviceID` overlap.
data Device = Device {
    deviceName :: String
  , deviceSize :: (X,Y)
  , deviceHost :: HostName   -- ^ Where it sends to.
  , devicePort :: Int        -- ^ Where it sends to.
  , devicePrefix :: String   -- ^ PITFALL: Includes a leading slash.
  , deviceRotation :: Int    -- ^ 0, 90, 180 or 270
  } deriving (Show, Eq, Ord)

data St = St {
    stInbox :: Socket
  , stToMonome :: Socket
  , stPending_Monome :: [(WindowLabel, ((X,Y), Bool))]
  , stVoices :: Map (X,Y) (Synth BoopParams)
    -- ^ TODO ? This is expensive, precluding the use of big synths.
    -- Maybe I could make them dynamically without much speed penalty.
    -- Tom of Vivid thinks so.
  , stPending_Vivid :: [(VoiceLabel,Float,String)]
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

data Window = Window {
    windowLabel :: WindowLabel -- ^ PITFALL: Must be unique across windows,
    -- or the Eq instance fails.
  , windowContains :: (X,Y) -> Bool
    -- ^ PITFALL: A monome will respond to out-of-bounds (x,y) values.
    -- Every Window therefore needs a nontrivial windowContains field,
    -- even the background Window.
  , windowInit :: St -> St
  , windowRoutine :: WindowRoutine
  }

instance Eq Window where
  (==) a b = windowLabel a == windowLabel b

-- | Acts on messages from the monome.
type WindowRoutine =
     St
  -> ((X,Y), Switch) -- ^ the incoming button press|release
  -> IO St
