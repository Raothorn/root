{-# LANGUAGE TemplateHaskell #-}
module Types.Faction.Eerie (
    -- Types
    BirdFaction,
    BirdAction(..),
    BirdPhase(..)
    -- Lenses
    -- Constructors
    -- Helpers
) where

import Lens.Micro.TH

import Types.Common
import Types.Faction.Common

----------------------------------
-- Types
----------------------------------
data BirdFaction = BirdFaction
    { _common :: FactionCommon
    , _decree :: Decree
    }

data Decree = Decree
    { _recruit :: [Card]
    , _move :: [Card]
    , _battle :: [Card]
    , _build :: [Card]
    }

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
-- Lenses
----------------------------------
makeLenses ''BirdFaction
makeLenses ''Decree
