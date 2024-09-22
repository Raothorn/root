{-# LANGUAGE TemplateHaskell #-}

module Types.RootBoard (
    -- Types
    RootBoard,
    -- Lenses
    clearings,
    -- Stateful functions
    getClearingsP
) where

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Types.Alias
import Types.Clearing (Clearing)
import qualified Types.Clearing as Clearing
import Types.IxTable as I
import Types.Common

----------------------------------
-- Types
----------------------------------
newtype RootBoard = RootBoard
    { _clearings :: IxTable Clearing
    }
    deriving (Show)

----------------------------------
-- Lenses
----------------------------------
makeLenses ''RootBoard
----------------------------------
-- Stateful functions
----------------------------------
getClearingsP :: (Clearing -> Bool) -> Update RootBoard [Clearing]
getClearingsP p = use (clearings . to I.values) <&> filter p
