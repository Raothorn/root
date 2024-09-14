module Types.Action (
    Action(..),
    isQuit,
) where

import Types.Location
import Types.Unit

data Action
    = NoAction
    | QuitAction
    | MoveUnit Direction
    | UnitAction UnitAction

isQuit :: Action -> Bool
isQuit QuitAction = True
isQuit _ = False
