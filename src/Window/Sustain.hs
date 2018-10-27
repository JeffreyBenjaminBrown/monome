{-# LANGUAGE DataKinds #-}

module Window.Sustain (
  sustainWindow
  ) where

import Control.Concurrent.MVar
import Data.Map as M
import Data.Set as S
import Vivid

import Types.App
import Types.Button
import Util.Network


handler :: MVar State -> LedRelay -> ((X,Y), Switch) -> IO ()
handler _   _        (_ , SwitchOff) = return ()
handler mst lr (xy, SwitchOn ) = do
  st <- takeMVar mst -- PITFALL: old state; has opposite sustain value.
  let color led xy = lr (xy, led)

  case sustainOn st of
    True -> do -- Sustain is off now. Free some voices, dark the led.
      putStrLn $ show $ sustained st
      let sy xy = (M.!) (voices st) xy
          quiet xy = set (sy xy) (0 :: I "amp")
      color LedOff xy
      mapM_ quiet $ S.difference (sustained st) (fingers st)
    False -> color LedOn xy >> return ()

  putMVar mst $ st { sustainOn = not $ sustainOn st
                   , sustained = if sustainOn st then S.empty
                     else fingers st  }

sustainWindow = Window {
  windowLabel = "sustainWindow"
  , windowContains = \(x,y) -> x == 0 && y == 0
  , windowHandler = handler
}
