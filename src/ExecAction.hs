module ExecAction (
    execAction,
) where

import Types

----------------------------------
-- ExecAction
----------------------------------
execAction :: Action -> Update Game ()
execAction _ = return ()

----------------------------------
-- ExecFactionAction
----------------------------------
execFactionAction :: FactionAction -> Update Game ()
----------------------------------
-- Marquis Actions
----------------------------------
