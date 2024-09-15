module ExecAction (
    execAction,
) where

import Control.Monad

import Lens.Micro
import Lens.Micro.Mtl

import Types
import Types.City (queueProduction)
import qualified Types.City as C
import qualified Types.Game as G
import Types.IxTable
import qualified Types.Location as L
import qualified Types.Unit as U
import Util

----------------------------------
-- ExecAction
----------------------------------
execAction :: Action -> Update Game ()
----------------------------------
-- MoveAction
----------------------------------
execAction (MoveUnit uid direction) = do
    -- Move the unit in the provided direction
    G.units . ixTable uid . U.location %= L.adjacentLocation direction

    -- Verify that the unit is in the bounds of the map
    unit' <- G.getUnit uid
    inBounds <- G.inMapBounds (unit' ^. U.location)
    unless inBounds $ liftErr MoveOutOfBoundsError
----------------------------------
-- UnitAction
----------------------------------
execAction (UnitAction uid ua) = do
    unit <- G.getUnit uid
    execUnitAction unit ua
----------------------------------
-- Queue Produciton
----------------------------------
execAction (QueueProduction cid ptype) = do
    zoom (G.cities . ixTable cid) $ queueProduction ptype
----------------------------------
-- NoAction
----------------------------------
execAction QuitAction = return ()
execAction NoAction = return ()

----------------------------------
-- ExecUnitAction
----------------------------------
execUnitAction :: Unit -> UnitAction -> Update Game ()
execUnitAction unit BuildCity = do
    let cityCon = C.newCity (unit ^. U.location)
    _ <- G.addIxEntry G.cities cityCon
    G.deleteIxEntry G.units (unit ^. U.unitId)
