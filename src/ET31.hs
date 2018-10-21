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
import Types.Button


et31 :: IO ()
et31 = do
  -- variables
  inbox <- receivesAt "127.0.0.1" 11111
  toMonome <- sendsTo (unpack localhost) 13993
  voices <- let places = [(a,b) | a <- [0..15], b <- [0..15]]
    in M.fromList . zip places <$> mapM (synth boop) (replicate 256 ())

  mapM (send toMonome . ledOsc "/monome" . (,LedOn)) $ enharmonicKeys (8,8)

  mailbox <- forkIO $ forever $ do
    eOsc <- decodeOSC <$> recv inbox 4096
    case eOsc of Left text -> putStrLn . show $ text
                 Right osc -> let p@(xy, _) = readSwitchOSC osc
                              in  playKey ((M.!) voices xy) p

  let loop :: IO ()
      loop = do cmd <- getChar
                case cmd of 'q' -> close inbox
                                   >> mapM_ free (M.elems voices)
                                   >> killThread mailbox
                            _   -> loop
  loop
