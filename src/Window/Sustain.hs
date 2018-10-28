{-# LANGUAGE DataKinds #-}

module Window.Sustain (
  sustainWindow
  , label
  ) where

import Control.Concurrent.MVar
import Data.Map as M
import Data.Set as S
import Vivid

import Types.Window
import Types.Button
import Types.State
import Util.Network


label = "sustain window"

theButton = (0,15)

sustainWindow = Window {
  windowLabel = label
  , windowContains = \(x,y) -> x == fst theButton && y == snd theButton
  , windowInit = \_ _ -> return ()
  , windowHandler = handler
}

handler :: MVar State -> LedRelay -> [Window] -> ((X,Y), Switch) -> IO ()
handler _   _  _ (_ , SwitchOff) = return ()
handler mst toSustainWindow _ (xy, SwitchOn ) = do
  st <- takeMVar mst -- PITFALL: old state; has opposite sustain value.
  let color led xy = toSustainWindow (xy, led)

  case sustainOn st of
    True -> do -- Sustain is off now. Free some voices, dark the led.
      let sy xy = (M.!) (voices st) xy
          quiet xy = set (sy xy) (0 :: I "amp")
      color LedOff xy
      mapM_ quiet $ S.difference (sustained st) (fingers st)
    False -> color LedOn xy >> return ()

  putMVar mst $ st { sustainOn = not $ sustainOn st
                   , sustained = if sustainOn st then S.empty
                     else fingers st  }
