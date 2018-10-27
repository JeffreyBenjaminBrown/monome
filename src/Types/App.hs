module Types.App where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent.MVar
import Vivid

import Synth
import ET31.Keyboard
import Types.Button
import Util.Network


-- | `LedRelay` is for preventing one Window from writing to
-- the `Led`s of another `Window`.
type LedRelay = ((X,Y), Led) -> IO ()
type LedFilter = ((X,Y), Led) -> Bool

belongsHere :: [Window] -> Window -> LedFilter
belongsHere ws w = f where
  above = takeWhile (/= w) ws -- the windows above `w`
  obscured :: (X,Y) -> Bool
  obscured xy = or $ map ($ xy) $ map windowContains above
  f :: ((X,Y), Led) -> Bool
  f (xy,_) = not (obscured xy) && windowContains w xy

playIfHere :: Socket -> [Window] -> Window -> LedRelay
playIfHere toMonome ws w = f where
  f :: ((X,Y),Led) -> IO ()
  f msg = if belongsHere ws w msg
    then (send toMonome $ ledOsc "/monome" msg) >> return ()
    else return ()

data State = State {
    inbox :: Socket
  , toMonome :: Socket
  , voices :: M.Map (X,Y) (Synth BoopParams)
  , anchor :: Int
  , shift :: Float -- ^ multiplicative; 2 = one octave higher
  , fingers :: S.Set (X,Y)
  , sustainOn :: Bool
  , sustained :: S.Set (X,Y)
  } deriving (Show, Eq)

data Window = Window {
    windowLabel :: String
  , windowContains :: (X,Y) -> Bool
  , windowHandler :: MVar State
--    -> LedRelay -- ^ Windows speaks to device through this
    -> ((X,Y), Switch) -- ^ the incoming button press|release
    -> IO ()
  }

instance Eq Window where
  (==) a b = windowLabel a == windowLabel b

handleSwitch :: [Window] -> MVar State -> ((X,Y), Switch) -> IO ()
handleSwitch []     _  _            = return ()
handleSwitch (w:ws) mst sw @ (xy,_) = do
  st <- readMVar mst
  case windowContains w xy of
    True -> windowHandler w mst sw
    False -> handleSwitch ws mst sw
