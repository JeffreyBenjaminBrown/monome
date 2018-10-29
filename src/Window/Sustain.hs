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

-- TODO (#feature) : erase (remove from 'lit', redraw keyboard) when it stops
-- TODO (#feature) "draw" (add to 'lit') a chord when it beccomes sustained
-- drawChord :: Set (X,Y)
--           -> Switch -- ^ the latest state of `sustained`
--           -> M.Map PitchClass LedReason
--           -> M.Map PitchClass LedReason
-- drawChord fingers SwitchOff = id

handler :: MVar State -> LedRelay -> [Window] -> ((X,Y), Switch) -> IO ()
handler _   _  _ (_ , SwitchOff) = return ()
handler mst toSustainWindow _ (xy, SwitchOn ) = do
  st <- takeMVar mst -- PITFALL: old state, opposite value of `sustainOn`
  let sustainOn' = not $ sustainOn st
      sustained' = if not sustainOn' then S.empty
                   else S.fromList $ M.toList $ fingers st

  -- redraw the sustain window, silence anything that needs it
  let drawSustainWindow = curry toSustainWindow xy
  case sustainOn' of
    False -> do -- Sustain is off now. Free some voices, dark the led.
      let quiet xy = set ((M.!) (voices st) xy) (0 :: I "amp")
      drawSustainWindow LedOff
      mapM_ quiet $
        S.difference (S.map fst $ sustained st)
                     (S.fromList $ M.keys $ fingers st)
    True -> drawSustainWindow LedOn >> return ()

  let st' = st { sustainOn = sustainOn'
               , sustained = sustained' }

  putMVar mst st'
