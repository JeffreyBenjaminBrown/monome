{-# LANGUAGE DataKinds #-}

module Window.Sustain (
  sustainWindow
  , label
  ) where

import Control.Concurrent.MVar
import Data.List as L
import Data.Map as M
import Data.Set as S
import Vivid

import Math31
import Types.Window
import Types.Button
import Types.State
import Util.Network
import Window.Common
import qualified Window.Keyboard


label = "sustain window"

theButton = (0,15)

sustainWindow = Window {
  windowLabel = label
  , windowContains = \(x,y) -> x == fst theButton && y == snd theButton
  , windowInit = \_ _ -> return ()
  , windowHandler = handler
}

--newLit :: Set (X,Y) -> Switch -> M.Map PitchClass LedReason
--                              -> M.Map PitchClass LedReason
--xnewLit _ SwitchOff = id

handler :: MVar State -> LedRelay -> [Window] -> ((X,Y), Switch) -> IO ()
handler _   _  _ (_ , SwitchOff) = return ()
handler mst toSustainWindow _ (xy, SwitchOn ) = do
  st <- takeMVar mst -- PITFALL: old state, opposite value of `sustainOn`
  let sustained' = if sustainOn st then S.empty else fingers st

  -- redraw the sustain window
  let draw = flip $ curry toSustainWindow
  case sustainOn st of
    True -> do -- Sustain is off now. Free some voices, dark the led.
      let sy xy = (M.!) (voices st) xy
          quiet xy = set (sy xy) (0 :: I "amp")
      draw LedOff xy
      mapM_ quiet $ S.difference (sustained st) (fingers st)
    False -> draw LedOn xy >> return ()

  let st' = st { sustainOn = not $ sustainOn st
               , sustained = sustained'
               }

  putMVar mst st'
