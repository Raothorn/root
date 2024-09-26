{-# LANGUAGE TemplateHaskell #-}

module Types.Faction.FactionCommon (
    -- Types
    Faction (..),
    FactionCommon,
    -- Constructors
    newFactionCommon,
    -- Lenses
    faction,
    warriors,
    hand,
    victoryPoints,
) where

import Lens.Micro
import Lens.Micro.TH

import Types.Card (Card)
import Types.Default
import Types.IxTable

----------------------------------
-- Types
----------------------------------
data Faction
    = Marquis
    | Eerie
    | NoFaction
    deriving (Show)

data FactionCommon = FactionCommon
    { _faction :: Faction
    , _warriors :: Int
    , _hand :: [Index Card]
    , _victoryPoints :: Int
    }
    deriving (Show)

----------------------------------
-- Instances
----------------------------------
instance Default FactionCommon where
    def =
        FactionCommon
            { _faction = NoFaction
            , _warriors = 0
            , _hand = []
            , _victoryPoints = 0
            }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''FactionCommon

----------------------------------
-- Constructors
----------------------------------
newFactionCommon :: Faction -> Int -> FactionCommon
newFactionCommon fac numWarriors = def & faction .~ fac & warriors .~ numWarriors
