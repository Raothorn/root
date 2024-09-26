module Types.Action (
    Action (..),
) where

import Types.Faction

data Action
    = NoAction
    | MarquisAction CatAction
    | EerieAction BirdAction
