{-# LANGUAGE TemplateHaskell #-}

module Types.Board (
    -- Types
    Board,
    -- Lenses
    clearings,
    -- Constructors
    newBoard,
) where

import Lens.Micro.TH

import Types.Clearing (Clearing)
import Types.Default
import Types.Index as I

----------------------------------
-- Types
----------------------------------
newtype Board = Board
    { _clearings :: IxList Clearing
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

----------------------------------
-- Constructors
----------------------------------
newBoard :: IxList Clearing -> Board
newBoard = Board
