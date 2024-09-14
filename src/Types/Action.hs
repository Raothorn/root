module Types.Action (
    execAction
) where

import Lens.Micro.Mtl

import qualified Util.MapUtil as MU

import Types.Alias
import Types.Game as G
import Types.Location

data Action
    = NoAction
    | MoveAction Direction

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
execAction (MoveAction direction) = do
    G.unit %= MU.adjacentLocation direction
