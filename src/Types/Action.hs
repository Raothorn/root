module Types.Action (
    Action(..),
) where


data Action
    = NoAction
    | Common CommonAction
    deriving (Show)

data CommonAction
    = Battle
    | Move
    | Craft
    deriving (Show)
