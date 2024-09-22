module Types.Action (
    Action(..),
) where

import Types.Faction

data Action
    = NoAction
    | Faction FactionAction
    deriving (Show)

