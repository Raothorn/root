{-# LANGUAGE TemplateHaskell #-}

module Types.Card (
    -- Types
    Card,
    -- Lenses
    suit,
    -- Constructors
    newCard
) where

import Lens.Micro.TH

import Types.Common

----------------------------------
-- Types
----------------------------------
data Card = Card
    { _suit :: Suit
    , _craftCost :: [Suit]
    }

----------------------------------
-- Instances
----------------------------------
----------------------------------
-- Constructors
----------------------------------
newCard :: Card
newCard = Card { _suit = Fox, _craftCost = [Fox, Mouse] }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Card
