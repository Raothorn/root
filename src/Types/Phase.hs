module Types.Phase (
    -- Types
    Phase (..),
    DayPhase (..),
) where

import Types.Faction

----------------------------------
-- Types
----------------------------------
data Phase
    = MarquisPhase CatPhase
    | EeriePhase BirdPhase
    | NoPhase

data DayPhase = Birdsong | Morning | Evening

----------------------------------
-- Instances
----------------------------------

----------------------------------
-- Helpers
----------------------------------
-- Returns the time of day (Birdsong, Morning, Evening)
-- given the more granular "Phase"
dayPhase :: Phase -> DayPhase
dayPhase _ = Birdsong
