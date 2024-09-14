module Types.Action (
    Action(..),
    isQuit,
) where

import Types.Location

data Action
    = NoAction
    | QuitAction
    | MoveUnit Direction

isQuit :: Action -> Bool
isQuit QuitAction = True
isQuit _ = False
