{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, OverloadedStrings
, TupleSections #-}

module ET31 (
  et31
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Monad (forever)
import qualified Data.Map as M
import Vivid
import Vivid.OSC

import ET31.Keyboard
import Util.Byte
import Util.Network
import Synth
import Types.App
import Types.Button


-- | Windows listed first are "on top of" later ones.
windows = [sustainWindow, shiftWindow, keyboardWindow]

keyboardWindow = Window {
  windowContains = const True
  , windowHandler =
    let f mst press @ (xy,_) = do
          st <- readMVar mst
          playKey ((M.!) (voices st) xy) (shift st) press
    in f
}

shiftWindow = Window {
  windowContains = \(x,y) -> numBetween x 0 1 && numBetween y 13 15
  , windowHandler =
    let f _   (_, SwitchOff) = return ()
        f mst (xy,SwitchOn ) = do
          let shiftInMicrotones = case xy of (0,15) -> 6
                                             (1,15) -> 31
                                             (0,14) -> -1
                                             (1,14) -> 1
                                             (0,13) -> -6
                                             (1,13) -> -31
          st <- takeMVar mst
          putMVar mst $ st {shift = shift st + shiftInMicrotones}
    in f
}

sustainWindow = Window {
  windowContains = \(x,y) -> x == 0 && y == 0
  , windowHandler = let
      f _   (_ , SwitchOff) = return ()
      f mst (xy, SwitchOn ) = do
        st <- takeMVar mst
        let colorIt led = send (toMonome st) $ ledOsc "/monome" (xy, led)
        case sustain st of True -> colorIt LedOff
                           False -> colorIt LedOn
        putMVar mst $ st {sustain = not $ sustain st}
    in f
}

guideposts :: Socket -> Led -> IO ()
guideposts toMonome led = mapM_ f $ enharmonicKeys (8,8)
  where f = send toMonome . ledOsc "/monome" . (,led)

et31 :: IO State
et31 = do
  inbox <- receivesAt "127.0.0.1" 11111
  toMonome <- sendsTo (unpack localhost) 13993
  voices <- let places = [(a,b) | a <- [0..15], b <- [0..15]]
    in M.fromList . zip places <$> mapM (synth boop) (replicate 256 ())
  mst <- newMVar $ State { inbox = inbox
                         , toMonome = toMonome
                         , voices = voices
                         , shift = 1
                         , sustain = False }

  guideposts toMonome LedOn

  mailbox <- forkIO $ forever $ do
    eOsc <- decodeOSC <$> recv inbox 4096
    case eOsc of Left text -> putStrLn . show $ text
                 Right osc ->
                   let switch = readSwitchOSC osc
                   in  handleSwitch windows mst switch

  let loop :: IO State
      loop = do cmd <- getChar
                case cmd of 'q' -> close inbox
                                   >> mapM_ free (M.elems voices)
                                   >> killThread mailbox
                                   >> guideposts toMonome LedOff
                                   >> readMVar mst >>= return
                            _   -> loop
  loop
