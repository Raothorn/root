module ExecAction (
    execAction,
) where

import Lens.Micro

import ExecAction.CatAction
import ExecAction.SetupAction
import qualified Root.Game as Game
import Root.Phase
import Root.Types
import Util

----------------------------------
-- ExecAction
----------------------------------
execAction :: Action -> Update Game ()
execAction action = do
    phase <- Game.getPhase
    execAction' phase action

execAction' :: Phase -> Action -> Update Game ()
execAction' (FactionSetupPhase phase) (SetupAction action) = do
    execSetupAction phase action
    Game.popPhase
execAction' (FactionTurnPhase phase) (TurnAction action) = do
    execTurnAction phase action

----------------------------------
-- Common Actions
----------------------------------
----------------------------------
-- Battle Actions
----------------------------------
execAction' (BattlePhase phaseData) (AttackerRoll roll) = do
    let phaseData' = phaseData & attackRoll ?~ roll
    Game.setPhase (BattlePhase phaseData')
execAction' (BattlePhase phaseData) (DefenderRoll roll) = do
    let phaseData' = phaseData & defendRoll ?~ roll
    Game.setPhase (BattlePhase phaseData')
execAction' _ _ = liftErr WrongPhase

----------------------------------
-- Turn Actions
----------------------------------
-- It is the responsibliity of the individual action dispatchers to pop the turn phase
execTurnAction :: FactionTurnPhase -> TurnAction -> Update Game ()
execTurnAction (MarquisPhase phase) (MarquisAction action) = execCatAction phase action
execTurnAction _ _ = liftErr WrongPhase
