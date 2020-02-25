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
import Monome.Types.Button
import Monome.Types.Initial
import Monome.Types.Window
import Monome.Util
import Monome.Window.Keyboard
import Monome.Window.Shift
import Monome.Window.Sustain


et31 :: Int -- ^ The monome address, as serialoscd reports on startup.
     -> IO St
et31 monomePort = do
  inbox :: Socket <-
    receivesAt "127.0.0.1" 8000
    -- I don't know why it's port 8000, or why it used to be 11111.
  toMonome :: Socket <-
    sendsTo (unpack localhost) monomePort
    -- to find the port number above, use the first part of HandTest.hs
  mst <- newMVar $ St {
      _stWindowLayers = [sustainWindow, shiftWindow, keyboardWindow]
    , _stToMonome = toMonome
    , _stVoices = mempty
    , _stPending_Vivid = []
    , _stPending_Monome = []

    , _stXyShift = (0,0)
    , _stFingers = mempty
    , _stLit = mempty
      -- M.singleton (2 :: PitchClass) $ S.singleton LedBecauseAnchor
    , _stSustained = mempty
    }

  initAllWindows mst
  responder <-
    forkIO $ forever $ do -- forever is inaccurate; `loop` below can kill it
    decodeOSC <$> recv inbox 4096 >>= \case
      Left text -> putStrLn . show $ text
      Right osc -> let switch = readSwitchOSC osc
                   in  handleSwitch mst switch

  let loop :: IO St =
        getChar >>= \case
        'q' -> do -- quit
          close inbox
          killThread responder
          st <- readMVar mst
          mapM_ (free . (^. voiceSynth)) (M.elems $ _stVoices st)
          _ <- send (_stToMonome st) $ allLedOsc "/monome" False
          return $ st { _stVoices = mempty }
        _   -> loop
    in putStrLn "press 'q' to quit"
       >> loop
