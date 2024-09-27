{-# LANGUAGE TemplateHaskell #-}

module Types.Card (
    -- Types
    Card,
    CardEffect (..),
    -- Lenses
    suit,
    craftCost,
    effect,
    -- Constructors
    newCard,
) where

import Lens.Micro.TH

import Types.CommonTypes
import Types.Default
import Types.IxTable

----------------------------------
-- Types
----------------------------------
data Card = Card
    { _index :: Index Card
    , _suit :: Suit
    , _craftCost :: [Suit]
    , _effect :: CardEffect
    }
    deriving (Show)

data CardEffect
    = VictoryPoints Int
    | NoEffect
    deriving (Show)

----------------------------------
-- Instances
----------------------------------
instance Indexed Card where
    getIx = _index

instance Default Card where
    def =
        Card
            { _index = def
            , _suit = def
            , _craftCost = []
            , _effect = def
            }

instance Default CardEffect where
    def = NoEffect

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Card

----------------------------------
-- Constructors
----------------------------------
newCard :: Suit -> [Suit] -> CardEffect -> Index Card -> Card
newCard s c e i = Card{_index = i, _suit = s, _craftCost = c, _effect = e}
