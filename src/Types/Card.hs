{-# LANGUAGE TemplateHaskell #-}

module Types.Card (
    -- Types
    Card,
    CardEffect (..),
    -- Lenses
    suit,
    craftCost,
    effect
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

data CardEffect
    = VictoryPoints Int
    | NoEffect

----------------------------------
-- Instances
----------------------------------
instance Indexed Card where
    getIx = _index

instance Default Card where
    def = Card 
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
