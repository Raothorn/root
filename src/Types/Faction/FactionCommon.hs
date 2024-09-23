{-# LANGUAGE TemplateHaskell #-}

module Types.Faction.FactionCommon (
    -- Types
    Faction(..),
    FactionCommon,
    -- Lenses
    faction,
    hand,
    victoryPoints,
) where

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Lookup.CardLookup (lookupCard)
import Types.Card (Card, CardEffect (..))
import qualified Types.Card as Card
import Types.Default
import Types.Error
import Types.IxTable

----------------------------------
-- Types
----------------------------------
data Faction
    = Marquis
    | Eerie
    | NoFaction

data FactionCommon = FactionCommon
    { _faction :: Faction
    , _hand :: [Index Card]
    , _victoryPoints :: Int
    }

----------------------------------
-- Instances
----------------------------------
instance Default FactionCommon where
    def =
        FactionCommon
            { _faction = NoFaction
            , _hand = []
            , _victoryPoints = 0
            }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''FactionCommon

