{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, LambdaCase
, OverloadedStrings
, TupleSections #-}

module ET31 (
  et31
  , colorArrows
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

colorAnchors :: Socket -> Int -> Led -> IO ()
colorAnchors toMonome anchor led = mapM_ f xy
  where xy = enharmonicToXYs $ et31ToLowXY anchor
        f = send toMonome . ledOsc "/monome" . (,led)

colorArrows :: Socket -> IO ()
colorArrows toMonome = mapM_ f [ (0,15),(0,14),(0,13)
                               , (1,14) ]
  where f = send toMonome . ledOsc "/monome" . (,LedOn) 

et31 :: IO State
et31 = do
  inbox <- receivesAt "127.0.0.1" 11111
  toMonome <- sendsTo (unpack localhost) 13993
  voices <- let places = [(a,b) | a <- [0..15], b <- [0..15]]
    in M.fromList . zip places <$> mapM (synth boop) (replicate 256 ())
  let initialAnchor = 2 :: Int
  mst <- newMVar $ State { inbox = inbox
                         , toMonome = toMonome
                         , voices = voices
                         , anchor = initialAnchor
                         , shift = 0
                         , fingers = S.empty
                         , sustainOn = False
                         , sustained = S.empty
                         }

  colorArrows toMonome
  colorAnchors toMonome initialAnchor LedOn

  responder <- forkIO $ forever $ do
    decodeOSC <$> recv inbox 4096 >>= \case
      Left text -> putStrLn . show $ text
      Right osc -> let switch = readSwitchOSC osc
                   in  handleSwitch windows mst switch

  let loop :: IO State
      loop = getChar >>= \case
        'q' -> do close inbox
                  mapM_ free (M.elems voices)
                  killThread responder
                  st <- readMVar mst
                  colorAnchors toMonome (anchor st) LedOff
                  return st
        _   -> loop
  loop
