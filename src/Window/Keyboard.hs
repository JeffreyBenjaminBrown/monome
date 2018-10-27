{-# LANGUAGE DataKinds #-}

module Window.Keyboard (
  keyboardWindow
  ) where

import Control.Concurrent.MVar
import Data.Set as S
import Data.Map as M
import Vivid

import Types.App
import Types.Button
import Util.Byte
import ET31.Keyboard


keyboardWindow =  Window {
  windowLabel = "keyboardWindow"
  , windowContains = const True
  , windowHandler = handler }

playKey :: State -> ((X,Y), Switch) -> IO ()
playKey st (xy, sw)
  | S.member xy (sustained st) = return ()
  | otherwise =
    let freq = 100 * (et31ToFreq $ shift st + xyToEt31 xy)
    in set ((M.!) (voices st) xy)
       (toI freq                         :: I "freq"
       , toI $ 0.15 * fi (switchToInt sw) :: I "amp")

handler :: MVar State -> ((X,Y), Switch) -> IO ()
handler mst press @ (xy,sw) = do
  st <- takeMVar mst
  playKey st press
  let newFingers = case sw of
        SwitchOn -> S.insert xy $ fingers st
        SwitchOff -> S.delete xy $ fingers st
  putMVar mst $ st { fingers = newFingers }
