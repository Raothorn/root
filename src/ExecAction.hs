module ExecAction (
    execAction,
) where

import Types

----------------------------------
-- ExecAction
----------------------------------
execAction :: Action -> Update Game ()
execAction _ = return ()
