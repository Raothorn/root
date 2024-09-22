{-# LANGUAGE TemplateHaskell #-}
module Types.Faction.Marquis (
    -- Types
    CatFaction,
    CatPhase(..),
    CatAction(..),
    -- Lenses
    -- Constructors
    -- Helpers
) where

import Lens.Micro.TH

import Types.Faction.Common
----------------------------------
-- Types
----------------------------------
data CatFaction = CatFaction 
    { _common :: FactionCommon
    , _sawmills :: Int
    , _workshops :: Int
    , _recruiters :: Int
    }
    deriving (Show)

data CatPhase 
    = CatPlaceWoodPhase
    | CatCraftPhase
    -- parameters: actionsLeft :: Int
    | CatChooseActionPhase Int
    | CatDrawPhase
    deriving (Show)

data CatAction 
    = CatPlaceWood
    | CatCraft
    | CatBattle
    | CatMarch
    | CatRecruit
    | CatBuild
    | CatOverwork
    | CatDraw
    deriving (Show)

----------------------------------
-- Lenses
----------------------------------
makeLenses ''CatFaction
