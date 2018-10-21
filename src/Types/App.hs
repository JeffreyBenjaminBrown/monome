module Types.App where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent.MVar
import Vivid

import Synth
import ET31.Keyboard
import Types.Button
import Util.Network


data State = State {
  inbox :: Socket
  , toMonome :: Socket
  , voices :: M.Map (X,Y) (Synth BoopParams)
  , shift :: Float -- ^ multiplicative; 2 = one octave higher
  }

data Window = Window {
  windowContains :: (X,Y) -> Bool
  , windowHandler :: MVar State -> ((X,Y), Switch) -> IO () }

type AppLeds = S.Set (X,Y)
type AppSwitches = S.Set (X,Y)

handleSwitch :: [Window] -> MVar State -> ((X,Y), Switch) -> IO ()
handleSwitch []     _  _            = return ()
handleSwitch (w:ws) mst sw @ (xy,_) = do
  st <- readMVar mst
  case windowContains w xy of
    True -> windowHandler w mst sw
    False -> handleSwitch ws mst sw
