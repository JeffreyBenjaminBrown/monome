module Monome.Types.Window (
    initAllWindows -- ^ MVar St -> [Window] -> IO ()
  , handleSwitch   -- ^ [Window] -> MVar St -> ((X,Y), Switch) -> IO ()

  -- | * only exported for the sake of testing
  , belongsHere    -- ^ [Window] -> Window -> LedFilter

-- | * So far, no need to export these.
--  LedRelay, LedFilter
--  , ledToWindow    -- ^ St -> [Window] -> (WindowId, ((X,Y), Led)) -> IO ()
--  , relayToWindow  -- ^ St -> WindowId -> [Window] -> LedRelay
--  , relayIfHere    -- ^ Socket > [Window] -> Window -> LedRelay
--  , findWindow     -- ^ [Window] -> WindowId -> Maybe Window
  ) where

import Prelude hiding (pred)
import Control.Concurrent.MVar
import qualified Data.List as L

import Monome.Network.Util
import Monome.Types.Button
import Monome.Types.Initial


-- | Forward a message to the monome if appropriate.
-- These are only used in this module.
type LedRelay  = ((X,Y), Led) -> IO ()
type LedFilter = (X,Y) -> Bool

initAllWindows :: MVar St -> [Window] -> IO ()
initAllWindows mst allWindows = do
  let runWindowInit :: St -> [Window] -> Window -> IO ()
      runWindowInit st ws w = let
        st' = windowInit w st
        in mapM_ (ledToWindow st' ws)
           $ stPending_Monome st'
  st <- readMVar mst
  mapM_ (runWindowInit st allWindows) allWindows

-- | called every time a monome button is pressed or released
handleSwitch :: [Window] -> MVar St -> ((X,Y), Switch) -> IO ()
handleSwitch    ws0         b          c =
  go ws0 b c where
  go :: [Window] -> MVar St -> ((X, Y), Switch) -> IO ()
  go    []          _           _            = return ()
  go    (w:ws)      mst         sw @ (btn,_) =
    case windowContains w btn of
      True -> do st0 <- takeMVar mst
                 st1 <- windowRoutine w st0 sw
                 -- TODO This should send to Vivid too.
                 mapM_ (ledToWindow st1 ws0) $ stPending_Monome st1
                 putMVar mst st1 {stPending_Monome = []}
      False -> go ws mst sw

ledToWindow :: St -> [Window] -> (WindowId, ((X,Y), Led)) -> IO ()
ledToWindow st ws (l, (xy,b)) =
  let toWindow = relayToWindow st l ws
  in toWindow (xy,b)

relayToWindow :: St -> WindowId -> [Window] -> LedRelay
relayToWindow st wl ws = let
  w = maybe err id $ findWindow ws wl
    where err = error $ "relayToWindow: " ++ wl ++ " not found."
  in relayIfHere (stToMonome st) ws w

-- | `relayIfHere dest ws w` returns a `LedRelay` which,
-- if the coordinate falls in `w` and in no other `Window` before `w` in `ws`,
-- sends the message to the `Socket`.
relayIfHere :: Socket -> [Window] -> Window -> LedRelay
relayIfHere dest ws w = f where
  f :: ((X,Y),Led) -> IO ()
  f msg = if belongsHere ws w $ fst msg
    then (send dest $ ledOsc "/monome" msg) >> return ()
    else return ()

-- | `belongsHere allWindows w _` returns a `Filter` that returns `True`
-- if `(X,Y)` belongs in `w` and none of the `Window`s preceding `w`.
-- PITFALL: `allWindows` should include literally all of them, even `w`.
belongsHere :: [Window] -> Window -> LedFilter
belongsHere allWindows w = f where
  obscurers = takeWhile (/= w) allWindows
    -- `obscurers` == the windows above `w`
  obscured :: (X,Y) -> Bool
  obscured xy = or $ map ($ xy) $ map windowContains obscurers
  f :: (X,Y) -> Bool
  f btn = not (obscured btn) && windowContains w btn

findWindow :: [Window] -> WindowId -> Maybe Window
findWindow ws l = L.find pred ws where
  -- Pitfall: Assumes the window will be found.
  pred = (==) l . windowLabel
