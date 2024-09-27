module Types.Phase (
    -- Types
    Phase (..),
    DayPhase (..),
    FactionSetupPhase (..),
    FactionTurnPhase (..),
) where

import Types.Default
import Types.Faction

----------------------------------
-- Types
----------------------------------
data Phase
    = -- Base phases
      SetupPhase [Faction]
    | TurnPhase Int
    | -- Other phases
      FactionSetupPhase FactionSetupPhase
    | FactionTurnPhase FactionTurnPhase
    | NoPhase
    deriving (Show)

data FactionSetupPhase
    = CatSetupPhase
    deriving (Show)

data FactionTurnPhase
    = MarquisPhase CatPhase
    | EeriePhase BirdPhase
    deriving (Show)

data DayPhase = Birdsong | Morning | Evening
    deriving (Show)

----------------------------------
-- Instances
----------------------------------
instance Default Phase where
    def = NoPhase

----------------------------------
-- Helpers
----------------------------------
-- Returns the time of day (Birdsong, Morning, Evening)
-- given the more granular "Phase"
dayPhase :: Phase -> DayPhase
dayPhase _ = Birdsong
