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
    -- Stateful functions
    addToken,
    -- Helpers
    hasToken,
    hasBuilding,
) where

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import Types.Alias
import Types.IxTable
import Types.Common

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
    deriving (Show)

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
-- Stateful functions
----------------------------------
addToken :: Token -> Update Clearing ()
addToken token = tokens %= (token :)

----------------------------------
-- Helpers
----------------------------------
hasToken :: Token -> Clearing -> Bool
hasToken token clearing = token `elem` clearing ^. tokens 

hasBuilding :: Building -> Clearing -> Bool
hasBuilding building clearing = building `elem` clearing ^. buildings
