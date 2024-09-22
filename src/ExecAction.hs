module ExecAction (
    execAction,
) where

import Lens.Micro.Mtl

import ExecAction.CatAction
import Types
import qualified Types.Game as Game
import Util

----------------------------------
-- ExecAction
----------------------------------
execAction :: Action -> Update Game ()
execAction _ = return ()

----------------------------------
-- ExecFactionAction
----------------------------------
execFactionAction :: FactionAction -> Update Game ()
execFactionAction (MarquisAction action) = do
    currentPhase <- use Game.phase
    case currentPhase of
        MarquisPhase phase -> execCatAction phase action
        _ -> liftErr NotFactionTurn
execFactionAction (EerieAction action) = liftErr NotImplemented
