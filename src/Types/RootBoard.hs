{-# LANGUAGE TemplateHaskell #-}

module Types.RootBoard (
    -- Types
    RootBoard,
    -- Lenses
    clearings,
    -- Stateful functions
    getClearingsP,
) where

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Types.Alias
import Types.Clearing (Clearing)
import qualified Types.Clearing as Clr
import Types.Common
import Types.Error
import Types.Faction
import Types.IxTable as I
import Util

----------------------------------
-- Types
----------------------------------
newtype RootBoard = RootBoard
    { _clearings :: IxTable Clearing
    }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''RootBoard

----------------------------------
-- Stateful functions
----------------------------------
getClearingsP :: (Clearing -> Bool) -> Update RootBoard [Clearing]
getClearingsP p = use (clearings . to I.values) <&> filter p
