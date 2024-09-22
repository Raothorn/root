module Types.Faction.Common (
    FactionCommon,
) where

import Types.Card

newtype FactionCommon = FactionCommon
    { _hand :: [Card]
    }
