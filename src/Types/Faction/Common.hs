module Types.Faction.Common (
    FactionCommon,
) where

import Types.Common

newtype FactionCommon = FactionCommon
    { _cards :: [Card]
    }
    deriving (Show)
