module ExecAction (
    execAction,
) where

import ExecAction.CatAction
import Root.Types
import qualified Root.Game as Game
import Util

----------------------------------
-- ExecAction
----------------------------------
execAction :: Action -> Update Game ()
execAction (Faction action) = execFactionAction action
execAction NoAction = liftErr EmptyTypeEncountered

----------------------------------
-- ExecFactionAction
----------------------------------
execFactionAction :: FactionAction -> Update Game ()
execFactionAction (MarquisAction action) = do
    currentPhase <- Game.getPhase
    case currentPhase of
        MarquisPhase phase -> execCatAction phase action
        _ -> liftErr NotFactionTurn
execFactionAction (EerieAction _) = liftErr NotImplemented
execFactionAction NoFactionAction = liftErr EmptyTypeEncountered
