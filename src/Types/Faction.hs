module Types.Faction (
    FactionAction (..),
    module Types.Faction.FactionCommon,
    module Types.Faction.Eerie,
    module Types.Faction.Marquis,
) where

import Types.Faction.FactionCommon
import Types.Faction.Eerie
import Types.Faction.Marquis

----------------------------------
-- Types
----------------------------------
data FactionAction
    = MarquisAction CatAction
    | EerieAction BirdAction
    | NoFactionAction
