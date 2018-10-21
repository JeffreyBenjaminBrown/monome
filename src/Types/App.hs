module Types.App where

import qualified Data.Map as M
import qualified Data.Set as S

import Types.Button


data Window = Window { windowContains :: (X,Y) -> Bool
                     , windowHandler :: ((X,Y), Switch) -> IO () }

type AppLeds = S.Set (X,Y)
type AppSwitches = S.Set (X,Y) -- maybe not needed

--ledDiff :: AppLeds -> AppLeds -> M.Map (X,Y) Led
--ledDiff new old = (turnOn, turnOff) where
