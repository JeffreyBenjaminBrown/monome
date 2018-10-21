module Types.App where

import qualified Data.Map as M
import qualified Data.Set as S

import Types.Button


type AppLeds = S.Set (X,Y)
type AppSwitches = S.Set (X,Y) -- maybe not needed

--ledDiff :: AppLeds -> AppLeds -> M.Map (X,Y) 
--ledDiff new old = (turnOn, turnOff) where
  
