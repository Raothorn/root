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
import Types.Default
import Types.IxTable as I

----------------------------------
-- Types
----------------------------------
newtype RootBoard = RootBoard
    { _clearings :: IxTable Clearing
    }

----------------------------------
-- Instances
----------------------------------
instance Default RootBoard where
    def =
        RootBoard
            { _clearings = I.empty
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
