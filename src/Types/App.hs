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
  , fingers :: S.Set (X,Y)
  , sustainOn :: Bool
  , sustained :: S.Set (X,Y)
  } deriving (Show, Eq)

data Window = Window {
  windowContains :: (X,Y) -> Bool
  , windowHandler :: MVar State -> ((X,Y), Switch) -> IO () }

handleSwitch :: [Window] -> MVar State -> ((X,Y), Switch) -> IO ()
handleSwitch []     _  _            = return ()
handleSwitch (w:ws) mst sw @ (xy,_) = do
  st <- readMVar mst
  case windowContains w xy of
    True -> windowHandler w mst sw
    False -> handleSwitch ws mst sw
