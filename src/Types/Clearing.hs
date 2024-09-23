{-# LANGUAGE TemplateHaskell #-}

module Types.Clearing (
    -- Types
    Clearing,
    -- Lenses
    adjacent,
    suit,
    buildingSlots,
    buildings,
    tokens,
    -- Constructor
    -- Helpers
    hasToken,
    hasBuilding,
) where

import Lens.Micro
import Lens.Micro.TH

import Types.IxTable
import Types.CommonTypes

----------------------------------
-- Types
----------------------------------
data Clearing = Clearing
    { _index :: Index Clearing
    , _adjacent :: [Index Clearing]
    , _suit :: Suit
    , _buildingSlots :: Int
    , _buildings :: [Building]
    , _tokens :: [Token]
    }

----------------------------------
-- Instances
----------------------------------
instance Indexed Clearing where
    getIx = _index

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Clearing

----------------------------------
-- Helpers
----------------------------------
hasToken :: Token -> Clearing -> Bool
hasToken token clearing = token `elem` clearing ^. tokens 

hasBuilding :: Building -> Clearing -> Bool
hasBuilding building clearing = building `elem` clearing ^. buildings
