module State.BoardState (
    getClearingsP
) where

import Lens.Micro
import Lens.Micro.Mtl

import Root.Types
import Types.Board
import qualified Types.IxTable as I
----------------------------------
-- Stateful functions
----------------------------------
getClearingsP :: (Clearing -> Bool) -> Update Board [Clearing]
getClearingsP p = use (clearings . to I.values) <&> filter p
