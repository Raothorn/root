{-# LANGUAGE TemplateHaskell #-}

module Types.Faction.Marquis (
    -- Types
    CatFaction,
    CatPhase (..),
    CatAction (..),
    -- Lenses
    common,
    woodTokens,
    -- Constructors
    -- Helpers
) where

import Lens.Micro.Mtl
import Lens.Micro.TH

import Types.Card
import Types.CommonTypes
import Types.Default
import Types.Faction.FactionCommon
import Types.IxTable

----------------------------------
-- Types
----------------------------------
data CatFaction = CatFaction
    { _common :: FactionCommon
    , _sawmills :: Int
    , _workshops :: Int
    , _recruiters :: Int
    , _woodTokens :: Int
    }

data CatPhase
    = CatPlaceWoodPhase
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
    , _woodTokens = 20
    }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''CatFaction

