{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, LambdaCase
, OverloadedStrings
, ScopedTypeVariables
, TupleSections #-}

module Monome.Main (
  et31
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Set as S
import Vivid
import Vivid.OSC

import Monome.Math31
import Monome.Util
import Monome.Network.Util
import Monome.Synth.Boop
import Monome.Types.Window
import Monome.Types.Button
import Monome.Types.State
import Monome.Window.Keyboard
import Monome.Window.Shift
import Monome.Window.Sustain


-- | PITFALL: Order matters.
-- Windows listed earlier are "above" later ones:
-- key presses are handled by the first window containing them.
windows :: [Window]
windows = [sustainWindow, shiftWindow, keyboardWindow]

et31 :: Maybe PitchClass -> IO State
et31 mbAnchor = do
  inbox <- receivesAt "127.0.0.1" 8000 -- I don't know why it's 8000.
    -- It used to be 11111; I don't know why that was, either.
  toMonome <- sendsTo (unpack localhost) 15226
    -- to find the right port number above, see HandTest.hs
  voices :: M.Map (X, Y) (Synth BoopParams) <-
    let places = [(a,b) | a <- [0..15], b <- [0..15]]
    in M.fromList . zip places <$> mapM (synth boop) (replicate 256 ())
  mst <- newMVar $ State {
      stInbox = inbox
    , stToMonome = toMonome
    , stVoices = voices
    , stXyShift = (0,0)
    , stFingers = mempty
    , stLit = let f anchor = M.singleton anchor $ S.singleton LedBecauseAnchor
              in maybe mempty f mbAnchor
    , stSustainOn = False
    , stSustained = mempty
    }

  runWindowInit mst windows

  responder <- forkIO $ forever $ do
    decodeOSC <$> recv inbox 4096 >>= \case
      Left text -> putStrLn . show $ text
      Right osc -> let switch = readSwitchOSC osc
                   in  handleSwitch windows mst switch

  let (loop :: IO State) = getChar >>= \case
        'q' -> do
          close inbox
          mapM_ free (M.elems voices)
          killThread responder
          st <- readMVar mst
          _ <- send toMonome $ allLedOsc "/monome" LedOff
          return $ st { stVoices = mempty } -- PITFALL: ?
        _   -> loop
  loop
