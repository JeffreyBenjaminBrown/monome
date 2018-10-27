{-# LANGUAGE TupleSections #-}

module Window.Shift (
  shiftWindow
  , colorAnchors
  , colorArrows
  ) where

import Control.Concurrent.MVar

import ET31.Keyboard
import Types.App
import Types.Button
import Util.Byte
import Util.Network


shiftWindow = Window {
  windowLabel = "shiftWindow"
  , windowContains = \(x,y) -> numBetween x 0 1 && numBetween y 13 15
  , windowHandler = handler
}

colorArrows :: Socket -> IO ()
colorArrows toMonome = mapM_ f [ (0,15),(0,14),(0,13)
                               , (1,14) ]
  where f = send toMonome . ledOsc "/monome" . (,LedOn) 

colorAnchors :: Socket -> Int -> Led -> IO ()
colorAnchors toMonome anchor led = mapM_ f xy
  where xy = enharmonicToXYs $ et31ToLowXY anchor
        f = send toMonome . ledOsc "/monome" . (,led)

handler _   (_, SwitchOff) = return ()
handler mst (xy,SwitchOn ) = do
  st <- takeMVar mst
  let anchorShift = case xy of (0,15) -> 6
                               (0,14) -> 1
                               (1,14) -> -1
                               (0,13) -> -6
                               _ -> 0
      pitchShift = case xy of (0,15) -> -6
                              (1,15) -> 31
                              (0,14) -> -1
                              (1,14) -> 1
                              (0,13) -> 6
                              (1,13) -> -31
      newAnchor = anchor st + anchorShift
  colorAnchors (toMonome st) (anchor st) LedOff
  colorAnchors (toMonome st) newAnchor LedOn
  putMVar mst $ st { shift = shift st + pitchShift
                   , anchor = mod newAnchor 31 }
