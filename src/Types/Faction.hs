module Types.Faction (
    Faction (..),
    FactionAction (..),
    module Types.Faction.Eerie,
    module Types.Faction.Marquis,
) where

import Types.Faction.Eerie
import Types.Faction.Marquis

----------------------------------
-- Types
----------------------------------
data Faction
    = Marquis CatFaction
    | Eerie BirdFaction

data FactionAction
    = MarquisAction CatAction
    | EerieAction BirdAction
    deriving (Show)


