State:
  stVoices :: M.Map (X, Y) (Synth BoopParams)

Main:
  voices :: M.Map (X, Y) (Synth BoopParams) <-
    let places = [(a,b) | a <- [0..15], b <- [0..15]]
    in M.fromList . zip places <$> mapM (synth boop) (replicate 256 ())
  ...
  mapM_ free (M.elems voices)

Window:
  handleSwitch:
    mapM_ (sendVivid st1)       $ stPending_Vivid st1

  sendVivid :: St -> (VoiceId, Float, String) -> IO ()
  sendVivid st (xy,f,"amp")  = set ((M.!) (stVoices st) xy) (toI f :: I "amp")
  ...
