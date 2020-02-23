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
import Control.Lens
import qualified Data.Map as M
--import qualified Data.Set as S
import Vivid
import Vivid.OSC

import Monome.Network.Util
import Monome.Synth.Boop
import Monome.Types.Button
import Monome.Types.Initial
import Monome.Types.Window
import Monome.Util
import Monome.Window.Keyboard
import Monome.Window.Shift
import Monome.Window.Sustain


initialPitch :: Pitch
initialPitch = 50

et31 :: Int -- ^ The monome address, as serialoscd reports on startup.
     -> IO St
et31 monomePort = do
  inbox :: Socket <- receivesAt "127.0.0.1" 8000
    -- I don't know why it's port 8000, or why it used to be 11111.
  toMonome :: Socket <- sendsTo (unpack localhost) monomePort
    -- to find the port number above, use the first part of HandTest.hs
  voices :: M.Map VoiceId Voice <-
    let voiceIds = [(a,b) | a <- [0..15], b <- [0..15]]
        defaultVoiceState s = Voice { _voiceSynth = s
                                    , _voicePitch = initialPitch
                                    , _voiceParams = mempty }
          -- `mempty` above is inaccurate -- initially each voice has
          -- amp 0 and freq 100, because those ares the `Boop` defaults.
          -- Since none are sounding, I don't think it matters.
    in M.fromList . zip voiceIds . map defaultVoiceState
       <$> mapM (synth boop) (replicate 256 ())
  mst <- newMVar $ St {
      _stWindowLayers = [sustainWindow, shiftWindow, keyboardWindow]
    , _stToMonome = toMonome
    , _stVoices = voices
    , _stPending_Vivid = []
    , _stPending_Monome = []

    , _stXyShift = (0,0)
    , _stFingers = mempty
    , _stLit = mempty
      -- M.singleton (2 :: PitchClass) $ S.singleton LedBecauseAnchor
    , _stSustainOn = False
    , _stSustained = mempty
    }

  initAllWindows mst

  responder <- forkIO $ forever $ do
    decodeOSC <$> recv inbox 4096 >>= \case
      Left text -> putStrLn . show $ text
      Right osc -> let switch = readSwitchOSC osc
                   in  handleSwitch mst switch

  let loop :: IO St =
        getChar >>= \case
        'q' -> do -- quit
          close inbox
          mapM_ (free . (^. voiceSynth)) (M.elems voices)
            -- TODO Once `voices` are dynamic,
            -- this should read that value from `mst`.
          killThread responder
          st <- readMVar mst
          _ <- send toMonome $ allLedOsc "/monome" False
          return $ st { _stVoices = mempty }
        _   -> loop
    in putStrLn "press 'q' to quit"
       >> loop
