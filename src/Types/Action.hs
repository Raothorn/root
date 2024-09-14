module Types.Action (
    Action(..),
    isQuit,
) where

import Types.City
import Types.Production
import Types.Location
import Types.Unit

data Action
    = NoAction
    | QuitAction
    | MoveUnit UnitId Direction
    | UnitAction UnitId UnitAction
    | QueueProduction CityId ProductionType

isQuit :: Action -> Bool
isQuit QuitAction = True
isQuit _ = False
