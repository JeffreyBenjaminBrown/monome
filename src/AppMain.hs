{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, LambdaCase
, OverloadedStrings
, ScopedTypeVariables
, TupleSections #-}

module AppMain (
  et31
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Monad (forever)
import qualified Data.Map as M
import qualified Data.Set as S
import Vivid
import Vivid.OSC

import Math31
import Util.Byte
import Util.Network
import Synth
import Types.Window
import Types.Button
import Types.State
import Window.Keyboard
import Window.Shift
import Window.Sustain


-- | PITFALL: Order matters.
-- Windows listed first are "on top of" later ones.
-- Key presses are handled by the first window containing them.
windows = [sustainWindow, shiftWindow, keyboardWindow]

et31 :: IO State
et31 = do
  inbox <- receivesAt "127.0.0.1" 11111
  toMonome <- sendsTo (unpack localhost) 13993
  voices <- let places = [(a,b) | a <- [0..15], b <- [0..15]]
    in M.fromList . zip places <$> mapM (synth boop) (replicate 256 ())
  mst <- newMVar $ State { inbox = inbox
                         , toMonome = toMonome
                         , voices = voices
                         , xyShift = (0,0)
                         , fingers = mempty
                         , lit = M.singleton 2 $ S.singleton LedFromAnchor
                         , sustainOn = False
                         , sustained = mempty
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
          send toMonome $ allLedOsc "/monome" LedOff
          return $ st { voices = mempty } -- PITFALL: less info, more reasable
        _   -> loop
  loop
