module ExecAction (
    execAction,
) where

import Lens.Micro
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
execAction (MoveUnit uid direction) =
    G.units . ixTable uid . U.location %= adjacentLocation direction
----------------------------------
-- UnitAction
----------------------------------
execAction (UnitAction uid ua) = do
    unit <- G.getUnit uid
    execUnitAction unit ua
----------------------------------
-- NoAction
----------------------------------
execAction _ = return ()

----------------------------------
-- ExecUnitAction
----------------------------------
execUnitAction :: Unit -> UnitAction -> Update Game ()
execUnitAction unit BuildCity = do
    G.cities %= (unit ^. U.location :)
    G.deleteUnit (unit ^. U.unitId)
