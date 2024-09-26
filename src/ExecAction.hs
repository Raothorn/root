module ExecAction (
    execAction,
) where

import ExecAction.SetupAction
import ExecAction.CatAction
import qualified Root.Game as Game
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
execAction' _ _ = liftErr WrongPhase

-- It is the responsibliity of the individual action dispatchers to pop the turn phase
execTurnAction :: FactionTurnPhase -> TurnAction -> Update Game ()
execTurnAction (MarquisPhase phase) (MarquisAction action) = execCatAction phase action
execTurnAction _ _ = liftErr WrongPhase

