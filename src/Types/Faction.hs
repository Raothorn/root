module Types.Faction (
    Faction(..),
    FactionAction(..),
) where

import Types.Faction.Marquis
import Types.Faction.Eerie
----------------------------------
-- Types
----------------------------------
data Faction 
    = Marquis CatFaction
    | Eerie BirdFaction

data Phase
    = MarquisPhase CatPhase
    | EeriePhase BirdPhase

data FactionAction 
    = MarquisAction CatAction
    | EerieAction
    deriving (Show)

data BirdPhase

data DayPhase = Birdsong | Morning | Evening

----------------------------------
-- Helpers
----------------------------------
-- Random functions that will need to be refiled eventually

-- Returns the time of day (Birdsong, Morning, Evening)
-- given the more granular "Phase"
dayPhase :: Phase -> DayPhase
dayPhase _ = Birdsong
