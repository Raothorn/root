{-# LANGUAGE TemplateHaskell #-}

module Types.Clearing (
    -- Types
    Clearing,
    -- Lenses
    index,
    adjacent,
    oppositeCorner,
    suit,
    buildingSlots,
    buildings,
    tokens,
    warriors,
    -- Constructor
    newClearing,
    -- Helpers
    isCorner,
    isAdjacent,
    hasToken,
    hasBuilding,
    hasWarrior,
) where

import Data.Maybe

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
    , _oppositeCorner :: Maybe (Index Clearing)
    , _suit :: Suit
    , _buildingSlots :: Int
    , _buildings :: [Building]
    , _tokens :: [Token]
    , _warriors :: [Warrior]
    }

----------------------------------
-- Instances
----------------------------------
instance Indexed Clearing where
    getIx = _index

instance Show Clearing where
    show clearing = "Clearing " <> show (getIx clearing)

----------------------------------
-- Constructors
----------------------------------
newClearing :: Index Clearing -> Clearing
newClearing index =
    Clearing
        { _index = index
        , _adjacent = []
        , _oppositeCorner = Nothing
        , _suit = def
        , _buildingSlots = 0
        , _buildings = []
        , _tokens = []
        , _warriors = []
        }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Clearing

----------------------------------
-- Helpers
----------------------------------
isCorner :: Clearing -> Bool
isCorner = isJust . _oppositeCorner

isAdjacent ::  Index Clearing -> Clearing -> Bool
isAdjacent other clearing = other `elem` clearing ^. adjacent

hasToken :: Token -> Clearing -> Bool
hasToken token clearing = token `elem` clearing ^. tokens

hasBuilding :: Building -> Clearing -> Bool
hasBuilding building clearing = building `elem` clearing ^. buildings

hasWarrior :: Warrior -> Clearing -> Bool
hasWarrior warrior clearing = warrior `elem` clearing ^. warriors
