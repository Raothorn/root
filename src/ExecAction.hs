module ExecAction (
    execAction
) where 

import Lens.Micro.Mtl

import qualified Util.MapUtil as MU

import Types
import qualified Types.Game as G


----------------------------------
-- ExecAction
----------------------------------
execAction :: Action -> Update Game ()
----------------------------------
-- NoAction
----------------------------------
execAction NoAction = return ()
----------------------------------
-- MoveAction
----------------------------------
execAction (MoveUnit direction) = do
    G.unit %= MU.adjacentLocation direction
