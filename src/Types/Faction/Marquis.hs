{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Faction.Marquis (
    -- Types
    CatFaction,
    CatPhase (..),
    CatAction (..),
    CatSetup,
    -- Lenses
    marquisCommon,
    sawmills,
    workshops,
    recruiters,
    woodTokens,
    buildings,
    keepCorner,
    sawmillClearing,
    workshopClearing,
    recruiterClearing,
    -- Helpers
) where

import Lens.Micro
import Lens.Micro.TH

import Parameters as P
import Types.Card (Card)
import Types.Clearing (Clearing)
import Types.CommonTypes
import Types.Default
import Types.Faction.FactionCommon
import Types.Index

----------------------------------
-- Types
----------------------------------
data CatFaction = CatFaction
    { _marquisCommon :: FactionCommon
    , _sawmills :: Int
    , _workshops :: Int
    , _recruiters :: Int
    , _woodTokens :: Int
    }
    deriving (Show)

data CatPhase
    = CatPlaceWoodPhase
    | -- parameters: workshopsUsed :: [Suit]
      CatCraftPhase [Suit]
    | -- parameters: actionsLeft :: Int, recruited :: Bool
      CatDaylightActionPhase Int Bool
    | CatDrawPhase
    | CatTurnEndedPhase
    deriving (Show)

data CatAction
    = CatPlaceWood
    | CatCraft (Index Card)
    | CatFinishCrafting
    | CatBattle (Index Clearing) Faction
    | CatMarch (Index Clearing) (Index Clearing) Int
    | CatRecruit
    | CatBuild (Index Clearing) Building [(Index Clearing, Int)]
    | CatOverwork (Index Card) (Index Clearing)
    | CatFinishDaylightActions
    | CatDraw

-- (Keep, Sawmil, Workshop, Recruiter)
type CatSetup = (Index Clearing, Index Clearing, Index Clearing, Index Clearing)

----------------------------------
-- Instances
----------------------------------
-- TODO move values to the Parameters module
instance Default CatFaction where
    def =
        CatFaction
            { _marquisCommon = newFactionCommon Marquis P.catWarriorSupply
            , _sawmills = P.catSawmills
            , _workshops = P.catWorkshops
            , _recruiters = P.catRecruiters
            , _woodTokens = P.catWoodTokens
            }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''CatFaction

buildings :: Building -> Lens' CatFaction Int
buildings Sawmill = sawmills
buildings Workshop = workshops
buildings Recruiter = recruiters

keepCorner :: Lens' CatSetup (Index Clearing)
keepCorner = _1

sawmillClearing :: Lens' CatSetup (Index Clearing)
sawmillClearing = _2

workshopClearing :: Lens' CatSetup (Index Clearing)
workshopClearing = _3

recruiterClearing :: Lens' CatSetup (Index Clearing)
recruiterClearing = _4
