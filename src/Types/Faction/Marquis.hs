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

import Types.IxTable
import Types.Card
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

data CatPhase
    = CatPlaceWoodPhase
    | -- parameters: suitsUsed :: [Suit]
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
-- Lenses
----------------------------------
makeLenses ''CatFaction

----------------------------------
-- Stateful functions
----------------------------------
-- Returns either an empty list or a list containing a single wood token.
-- Since the monoid instance of Maybe doesn't really work, we use this
-- as a workaround for any stateful function with a non-empty return type
-- so that we can easily use these within a "zoomed" context.
takeWoodToken :: Update CatFaction [Token]
takeWoodToken = do
    remainingWood <- use woodTokens
    if remainingWood > 0
        then do
            woodTokens -= 1
            return [Wood]
        else return []
