module Types.Action (
    Action (..),
    SetupAction (..),
    TurnAction (..),
) where

import Types.Faction

data Action
    = NoAction
    | SetupAction SetupAction
    | TurnAction TurnAction
    | -- Common actions
      AttackerRoll Int
    | DefenderRoll Int

data SetupAction
    = CatSetupAction CatSetup
    | NoSetupAction

data TurnAction
    = MarquisAction CatAction
    | EerieAction BirdAction
