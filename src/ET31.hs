{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, LambdaCase
, OverloadedStrings
, TupleSections #-}

module ET31 (
  et31
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Monad (forever)
import qualified Data.Map as M
import qualified Data.Set as S
import Vivid
import Vivid.OSC

import ET31.Keyboard
import Util.Byte
import Util.Network
import Synth
import Types.App
import Types.Button


-- | PITFALL: Order matters.
-- Windows listed first are "on top of" later ones.
-- Key presses are handled by the first window containing them.
windows = [sustainWindow, shiftWindow, keyboardWindow]

keyboardWindow = let
  playKey :: State -> ((X,Y), Switch) -> IO ()
  playKey st (xy, sw)
    | S.member xy (sustained st) = return ()
    | otherwise =
      let freq = 100 * (et31ToFreq $ shift st + xyToEt31 xy)
      in set ((M.!) (voices st) xy)
         (toI freq                         :: I "freq"
         , toI $ 0.15 * fi (switchToInt sw) :: I "amp")

  windowHandler mst press @ (xy,sw) = do
    st <- takeMVar mst
    playKey st press
    let newFingers = case sw of
          SwitchOn -> S.insert xy $ fingers st
          SwitchOff -> S.delete xy $ fingers st
    putMVar mst $ st { fingers = newFingers }

  in Window { windowContains = const True
            , windowHandler = windowHandler }

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
        st <- takeMVar mst -- PITFALL: old state; has opposite sustain value.
        let color led xy = send (toMonome st) $ ledOsc "/monome" (xy, led)

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
    in f
}

guideposts :: Socket -> Led -> IO ()
guideposts toMonome led = mapM_ f $ enharmonicToXY (9,0)
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
                         , fingers = S.empty
                         , sustainOn = False
                         , sustained = S.empty
                         }

  guideposts toMonome LedOn

  responder <- forkIO $ forever $ do
    decodeOSC <$> recv inbox 4096 >>= \case
      Left text -> putStrLn . show $ text
      Right osc -> let switch = readSwitchOSC osc
                   in  handleSwitch windows mst switch

  let loop :: IO State
      loop = getChar >>= \case
        'q' -> close inbox
               >> mapM_ free (M.elems voices)
               >> killThread responder
               >> guideposts toMonome LedOff
               >> readMVar mst >>= return
        _   -> loop
  loop
