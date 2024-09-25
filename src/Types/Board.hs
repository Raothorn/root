{-# LANGUAGE TemplateHaskell #-}
module Types.Board (
    -- Types
    Board,
    -- Lenses
    clearings,
) where

import Lens.Micro.TH

import Types.Clearing (Clearing)
import Types.Default
import Types.IxTable as I

----------------------------------
-- Types
----------------------------------
newtype Board = Board
    { _clearings :: IxTable Clearing
    }

----------------------------------
-- Instances
----------------------------------
instance Default Board where
    def =
        Board
            { _clearings = I.empty
            }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Board

