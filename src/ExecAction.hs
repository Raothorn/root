module ExecAction (
    execAction,
) where

import ExecAction.CatAction
import qualified Root.Game as Game
import Root.Types

----------------------------------
-- ExecAction
----------------------------------
execAction :: Action -> Update Game ()
execAction action = do
    phase <- Game.getPhase
    execAction' phase action





execAction' :: Phase -> Action -> Update Game ()
execAction' (MarquisPhase phase) (MarquisAction action) = execCatAction phase action
execAction' _ _ = return ()
