{-# LANGUAGE DataKinds #-}

module Window.Keyboard (
  keyboardWindow
  , label
  ) where

import Control.Concurrent.MVar
import Data.Set as S
import Data.Map as M
import Vivid

import Synth
import Types.Window
import Types.Button
import Types.State
import Util.Byte
import Math31
import Window.Common (colorAnchors)


label = "keyboard window"

keyboardWindow =  Window {
  windowLabel = label
  , windowContains = const True
  , windowInit = \mst toKeyboard ->
      do a <- anchor <$> readMVar mst
         colorAnchors toKeyboard a LedOn
  , windowHandler = handler }

playKey :: State -> ((X,Y), Switch) -> IO ()
playKey st (xy, sw)
  | S.member xy (sustained st) = return ()
  | otherwise =
    let freq = 100 * (et31ToFreq $ shift st + xyToEt31 xy)
        voice = (M.!) (voices st) xy
    in set voice ( toI freq                         :: I "freq"
                 , toI $ 0.15 * fi (switchToInt sw) :: I "amp" )

handler :: MVar State
  -> LedRelay
  -> [Window]
  -> ((X,Y), Switch) -> IO ()
handler mst _ _ press @ (xy,sw) = do
  st <- takeMVar mst
  playKey st press
  let newFingers = case sw of
        SwitchOn -> S.insert xy $ fingers st
        SwitchOff -> S.delete xy $ fingers st
  putMVar mst $ st { fingers = newFingers }
