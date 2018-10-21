{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, OverloadedStrings
, TupleSections #-}

module ET31 where

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


enharmonicKeys :: (X,Y) -> [(X,Y)]
enharmonicKeys (x,y) = let contained x = x <= 15 && x >= 0
  in filter (\(x,y) -> contained x && contained y)
     $ [(x + x' * 6 + y' * (-1), y + x' * 1 + y' * 5 )
       | x' <- [-2..2], y' <- [-3..3] ]

et31 :: IO ()
et31 = do
  inbox <- receivesAt "127.0.0.1" 11111
  toMonome <- sendsTo (unpack localhost) 13993
  mapM (send toMonome . ledOsc "/monome" . (,LedOn)) $ enharmonicKeys (8,8)
  let places = [(a,b) | a <- [0..15], b <- [0..15]]
  voices <- M.fromList . zip places <$> mapM (synth boop) (replicate 256 ())
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
