module Monome.Types.Window where

import Control.Concurrent.MVar

import Monome.Types.Button
import Monome.Types.State
import Monome.Util.Network


-- | `LedRelay` is for preventing one Window from writing to
-- the `Led`s of another `Window`.
type LedRelay = ((X,Y), Led) -> IO ()
type LedFilter = ((X,Y), Led) -> Bool

-- | PITFALL: `allWindows` should be literally all of them, including `w`.
belongsHere :: [Window] -> Window -> LedFilter
belongsHere allWindows w = f where
  obscurers = takeWhile (/= w) allWindows
    -- `obscurers` == the windows above `w`
  obscured :: (X,Y) -> Bool
  obscured xy = or $ map ($ xy) $ map windowContains obscurers
  f :: ((X,Y), Led) -> Bool
  f (btn,_) = not (obscured btn) && windowContains w btn

relayIfHere :: Socket -- ^ probably to the monome
            -> [Window] -> Window -> LedRelay
relayIfHere dest ws w = f where
  f :: ((X,Y),Led) -> IO ()
  f msg = if belongsHere ws w msg
    then (send dest $ ledOsc "/monome" msg) >> return ()
    else return ()

type WindowLabel = String

data Window = Window {
    windowLabel :: WindowLabel
  , windowContains :: (X,Y) -> Bool
    -- ^ PITFALL: A monome will respond to out-of-bounds (x,y) values.
    -- Every Window therefore needs a nontrivial windowContains field,
    -- even the background Window.
  , windowInit :: MVar State -> LedRelay -> IO ()
  , windowHandler -- ^ Acts on messages from the monome.
    :: MVar State
    -> LedRelay -- ^ control Leds via this, not raw `send` commands
    -> [Window] -- ^ to construct an LedRelay to another Window, if needed
      -- PIFALL: Should be a list of all Windows -- not just, say, later ones.
    -> ((X,Y), Switch) -- ^ the incoming button press|release
    -> IO ()
  }

instance Eq Window where
  (==) a b = windowLabel a == windowLabel b

runWindowInit :: MVar State -> [Window] -> IO ()
runWindowInit mst allWindows = do
  st <- readMVar mst
  let toWindow w = relayIfHere (stToMonome st) allWindows w
  mapM_ (\w -> windowInit w mst $ toWindow w) allWindows

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
              in windowHandler w mst ledRelay allWindows sw
      False -> go allWindows ws mst sw
