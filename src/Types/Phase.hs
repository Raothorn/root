{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Types.Phase (
    -- Types
    Phase (..),
    DayPhase (..),
    FactionSetupPhase (..),
    FactionTurnPhase (..),
    BattlePhaseData,
    -- Lenses
    attacker,
    defender,
    clearing,
    attackRoll,
    defendRoll,
    -- Constructors
    newBattlePhase,
) where

import Lens.Micro.TH

import Types.Clearing
import Types.Default
import Types.Faction
import Types.Index

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
    | BattlePhase BattlePhaseData
    | NoPhase
    deriving (Show)

data FactionSetupPhase
    = CatSetupPhase
    | BirdSetupPhase
    deriving (Show)

data FactionTurnPhase
    = MarquisPhase CatPhase
    | EeriePhase BirdPhase
    deriving (Show)

data BattlePhaseData = BattlePhaseData
    { _attacker :: Faction
    , _defender :: Faction
    , _clearing :: Index Clearing
    , _attackRoll :: Maybe Int
    , _defendRoll :: Maybe Int
    }
    deriving (Show)

data DayPhase = Birdsong | Morning | Evening
    deriving (Show)

----------------------------------
-- Lenses
----------------------------------
makeLenses ''BattlePhaseData

----------------------------------
-- Instances
----------------------------------
instance Default Phase where
    def = NoPhase

----------------------------------
-- Constructors
----------------------------------
newBattlePhase :: Faction -> Faction -> Index Clearing -> Phase
newBattlePhase attacker defender clearing =
    BattlePhase $
        BattlePhaseData
            { _attacker = attacker
            , _defender = defender
            , _clearing = clearing
            , _attackRoll = Nothing
            , _defendRoll = Nothing
            }

----------------------------------
-- Helpers
----------------------------------
-- Returns the time of day (Birdsong, Morning, Evening)
-- given the more granular "Phase"
dayPhase :: Phase -> Maybe DayPhase
dayPhase _ = Nothing
