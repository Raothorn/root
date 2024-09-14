module ExecAction (
    execAction,
) where

import Lens.Micro.Mtl

import Util.MapUtil

import Types
import qualified Types.Game as G
import qualified Types.Unit as U

----------------------------------
-- ExecAction
----------------------------------
execAction :: Action -> Update Game ()
----------------------------------
-- MoveAction
----------------------------------
execAction (MoveUnit direction) = do
    G.unit . U.location %= adjacentLocation direction
----------------------------------
-- UnitAction
----------------------------------
execAction (UnitAction ua) = execUnitAction ua
----------------------------------
-- NoAction
----------------------------------
execAction _ = return ()

----------------------------------
-- ExecUnitAction
----------------------------------
execUnitAction :: UnitAction -> Update Game ()
execUnitAction BuildCity = do
    unitLocation <- use $ G.unit . U.location
    G.cities %= (unitLocation :)
