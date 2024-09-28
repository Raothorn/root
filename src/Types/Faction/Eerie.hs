{-# LANGUAGE TemplateHaskell #-}

module Types.Faction.Eerie (
    -- Types
    BirdFaction,
    BirdAction (..),
    BirdPhase (..),
    -- Lenses
    eerieCommon,
    -- Constructors
    -- Helpers
) where

import Lens.Micro.TH

import Types.Card
import Types.Default
import Types.Faction.FactionCommon

----------------------------------
-- Types
----------------------------------
data BirdFaction = BirdFaction
    { _eerieCommon :: FactionCommon
    , _decree :: Decree
    }
    deriving (Show)

data Decree = Decree
    { _recruit :: [Card]
    , _move :: [Card]
    , _battle :: [Card]
    , _build :: [Card]
    }
    deriving (Show)

data BirdPhase
    = BirdFirstDrawPhase
    | BirdAddCardsToDecreePhase
    | BirdPlaceRoostPhase
    | BirdCraftPhase
    | BirdResolveDecreePhase
    | BirdSecondDrawPhase
    deriving (Show)

data BirdAction
    = BirdDraw
    | BirdAddCardToDecree
    | BirdPlaceRoost
    | BirdCraft
    | BirdTakeDecreeAction
    | BirdDrawAndDiscard
    deriving (Show)

----------------------------------
-- Instances
----------------------------------
instance Default BirdFaction where
    def =
        BirdFaction
            { _eerieCommon = def
            , _decree = def
            }

instance Default Decree where
    def =
        Decree
            { _recruit = []
            , _move = []
            , _battle = []
            , _build = []
            }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''BirdFaction
makeLenses ''Decree
