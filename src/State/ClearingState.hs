module State.ClearingState (
    addToken,
) where

import Lens.Micro.Mtl

import Root.Types
import Types.Clearing

----------------------------------
-- Stateful functions
----------------------------------
addToken :: Token -> Update Clearing ()
addToken token = tokens %= (token :)
