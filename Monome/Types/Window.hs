module Monome.Types.Window where

import Prelude hiding (pred)
import Control.Concurrent.MVar
import qualified Data.List as L

import Monome.Types.Button
import Monome.Types.State
import Monome.Network.Util


type LedRelay  = ((X,Y), Led) -> IO ()
type LedFilter = ((X,Y), Led) -> Bool

-- | `belongsHere allWindows w _` returns a `Filter` that returns `True`
-- if `(X,Y)` belongs in `w` and none of the `Window`s preceding `w`.
-- PITFALL: `allWindows` should include literally all of them, even `w`.
belongsHere :: [Window] -> Window -> LedFilter
belongsHere allWindows w = f where
  obscurers = takeWhile (/= w) allWindows
    -- `obscurers` == the windows above `w`
  obscured :: (X,Y) -> Bool
  obscured xy = or $ map ($ xy) $ map windowContains obscurers
  f :: ((X,Y), Led) -> Bool
  f (btn,_) = not (obscured btn) && windowContains w btn

-- | `relayIfHere dest ws w` returns a `LedRelay` which,
-- if the coordinate falls in `w` and in no other `Window` before `w` in `ws`,
-- sends the message to the `Socket`.
relayIfHere :: Socket
            -> [Window] -> Window -> LedRelay
relayIfHere dest ws w = f where
  f :: ((X,Y),Led) -> IO ()
  f msg = if belongsHere ws w msg
    then (send dest $ ledOsc "/monome" msg) >> return ()
    else return ()

type WindowLabel = String

data Window = Window {
    windowLabel :: WindowLabel -- ^ PITFALL: Must be unique across windows,
    -- or the Eq instance fails.
  , windowContains :: (X,Y) -> Bool
    -- ^ PITFALL: A monome will respond to out-of-bounds (x,y) values.
    -- Every Window therefore needs a nontrivial windowContains field,
    -- even the background Window.
  , windowInit :: MVar State -> LedRelay -> IO ()
  , windowRoutine -- ^ Acts on messages from the monome.
    :: MVar State
    -> LedRelay -- ^ Control Leds via this, not raw `send` commands.
    -> [Window] -- ^ To construct an LedRelay to another Window, if needed.
      -- PIFALL: Should be a list of all Windows -- not just, say, later ones.
    -> ((X,Y), Switch) -- ^ the incoming button press|release
    -> IO ()
  }

instance Eq Window where
  (==) a b = windowLabel a == windowLabel b

initAllWindows :: MVar State -> [Window] -> IO ()
initAllWindows mst allWindows = do
  st <- readMVar mst
  let toWindow w = relayIfHere (stToMonome st) allWindows w
  mapM_ (\w -> windowInit w mst $ toWindow w) allWindows

-- | called every time a monome button is pressed or released
handleSwitch     :: [Window] -> MVar State -> ((X,Y), Switch) -> IO ()
handleSwitch a b c =
  go       a a b c where
  -- `go` keeps the complete list of windows in its first arg,
  -- while iteratively discarding the head of its second.
  go :: [Window] -> [Window] -> MVar State -> ((X, Y), Switch) -> IO ()
  go    _           []          _             _            = return ()
  go    allWindows  (w:ws)      mst           sw @ (btn,_) = do
    st <- readMVar mst
    case windowContains w btn of
      True -> let ledRelay = relayIfHere (stToMonome st) allWindows w
              in windowRoutine w mst ledRelay allWindows sw
      False -> go allWindows ws mst sw

findWindow :: [Window] -> WindowLabel -> Maybe Window
findWindow ws l = L.find pred ws where
  -- Pitfall: Assumes the window will be found.
  pred = (==) l . windowLabel
