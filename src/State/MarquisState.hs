module State.MarquisState (
    takeWoodToken
) where

import Lens.Micro.Mtl

import Root.Types
import qualified Types.Faction.Marquis as Cat
----------------------------------
-- Stateful functions
----------------------------------
{-
    Parameters: none
    Errors: none
    Returns: either an empty list or a list containing a single wood token.
-}
takeWoodToken :: Update CatFaction [Token]
takeWoodToken = do
    remainingWood <- use Cat.woodTokens
    if remainingWood > 0
        then do
            Cat.woodTokens -= 1
            return [Wood]
        else return []
