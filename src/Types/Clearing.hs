{-# LANGUAGE TemplateHaskell #-}

module Types.Clearing (
    -- Types
    Clearing,
    -- Lenses
    adjacent,
    isCorner,
    suit,
    buildingSlots,
    buildings,
    tokens,
    -- Constructor
    newClearing,
    -- Helpers
    hasToken,
    hasBuilding,
) where

import Lens.Micro
import Lens.Micro.TH

import Types.CommonTypes
import Types.Default
import Types.IxTable

----------------------------------
-- Types
----------------------------------
data Clearing = Clearing
    { _index :: Index Clearing
    , _adjacent :: [Index Clearing]
    , _isCorner :: Bool
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
-- Constructors
----------------------------------
newClearing :: Index Clearing -> Clearing
newClearing index =
    Clearing
        { _index = index
        , _adjacent = []
        , _isCorner = False
        , _suit = def
        , _buildingSlots = 0
        , _buildings = []
        , _tokens = []
        }

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
