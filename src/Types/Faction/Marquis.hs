{-# LANGUAGE TemplateHaskell #-}

module Types.Faction.Marquis (
    -- Types
    CatFaction,
    CatPhase (..),
    CatAction (..),
    -- Lenses
    -- Constructors
    -- Stateful functions
    takeWoodToken,
    -- Helpers
) where

import Lens.Micro.Mtl
import Lens.Micro.TH

import Types.Alias
import Types.Common
import Types.Faction.Common

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
    deriving (Show)

data CatPhase
    = CatPlaceWoodPhase
    | CatCraftPhase
    | -- parameters: actionsLeft :: Int
      CatChooseActionPhase Int
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

----------------------------------
-- Stateful functions
----------------------------------
takeWoodToken :: Update CatFaction (Maybe Token)
takeWoodToken = do
    remainingWood <- use woodTokens
    if remainingWood > 0
        then do
            woodTokens -= 1
            return (Just Wood)
        else return Nothing
