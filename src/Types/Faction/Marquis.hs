{-# LANGUAGE TemplateHaskell #-}

module Types.Faction.Marquis (
    -- Types
    CatFaction,
    CatPhase (..),
    CatAction (..),
    CatSetup,
    -- Lenses
    common,
    woodTokens,
    keepCorner,
    sawmillClearing,
    workshopClearing,
    recruiterClearing,
    -- Constructors
    -- Helpers
) where

import Lens.Micro.Mtl
import Lens.Micro.TH

import Types.Card (Card)
import Types.Clearing (Clearing)
import Types.CommonTypes
import Types.Default
import Types.Faction.FactionCommon (FactionCommon)
import Types.IxTable

----------------------------------
-- Types
----------------------------------
data CatFaction = CatFaction
    { _common :: FactionCommon
    , _sawmills :: Int
    , _workshops :: Int
    , _recruiters :: Int
    , _warriors :: Int
    , _woodTokens :: Int
    }

data CatPhase
    = CatSetupPhase
    | CatPlaceWoodPhase
    | -- parameters: workshopsUsed :: [Suit]
      CatCraftPhase [Suit]
    | -- parameters: actionsLeft :: Int
      CatChooseActionPhase Int
    | CatDrawPhase

data CatAction
    = CatPlaceWood
    | CatCraft (Index Card)
    | CatBattle
    | CatMarch
    | CatRecruit
    | CatBuild
    | CatOverwork
    | CatDraw

data CatSetup = CatSetup 
    { _keepCorner :: Index Clearing
    , _sawmillClearing :: Index Clearing
    , _workshopClearing :: Index Clearing
    , _recruiterClearing :: Index Clearing
    }

----------------------------------
-- Instances
----------------------------------
-- TODO verify values
instance Default CatFaction where
  def = CatFaction
    { _common = def
    , _sawmills = 5
    , _workshops = 5
    , _recruiters = 5
    , _warriors = 25
    , _woodTokens = 8
    }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''CatFaction
makeLenses ''CatSetup
