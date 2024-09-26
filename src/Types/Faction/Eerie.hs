{-# LANGUAGE TemplateHaskell #-}

module Types.Faction.Eerie (
    -- Types
    BirdFaction,
    BirdAction (..),
    BirdPhase (..),
    -- Lenses
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

data BirdAction
    = BirdDraw
    | BirdAddCardToDecree
    | BirdPlaceRoost
    | BirdCraft
    | BirdTakeDecreeAction
    | BirdDrawAndDiscard

----------------------------------
-- Instances
----------------------------------
instance Default BirdFaction where
    def =
        BirdFaction
            { _common = def
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
