-- Testing whether Vivid and SuperCollider are fast enough
-- to create and destroy voices in realtime.

{-# LANGUAGE DataKinds
, LambdaCase
, ScopedTypeVariables #-}

module Monome.JitTest where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Data.ByteString.Char8 (unpack)
import qualified Data.Map as M
import Vivid
import Vivid.OSC

import Monome.Network.Util
import Monome.Synth.Zot
import Monome.Types.Button
import Monome.Types.Device


jit :: IO ()
jit = do
  inbox :: Socket <- receivesAt "127.0.0.1" 8000
  toMonome <- sendsTo (unpack localhost) 14718
  mm :: MVar (M.Map (X,Y) (Synth BoopParams)) <- newMVar mempty

  responder <- forkIO $ forever $ do
    decodeOSC <$> recv inbox 4096 >>= \case
      Left text -> putStrLn . show $ text
      Right osc -> let
        ((x,y),switch) = readSwitchOSC osc
        in if switch
           then do s <- synth boop ( toI 0.05             :: I "amp"
                                   , toI (20*(x+1)*(y+1)) :: I "freq")
                   modifyMVar_ mm $ \m -> return $ M.insert (x,y) s m
           else do modifyMVar_ mm $ \m -> free (m M.! (x,y)) >>
                                          return (M.delete (x,y) m)

  let loop :: IO () =
        getChar >>= \case
        'q' -> do -- quit
          readMVar mm >>= \m -> mapM_ free $ M.elems m
          close inbox
          killThread responder
        _   -> loop
    in putStrLn "press 'q' to quit"
       >> loop
