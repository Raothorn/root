module Types.Action (
    Action(..),
    isQuit,
) where

import Types.Location
import Types.Unit

data Action
    = NoAction
    | QuitAction
    | MoveUnit UnitId Direction
    | UnitAction UnitId UnitAction

isQuit :: Action -> Bool
isQuit QuitAction = True
isQuit _ = False
