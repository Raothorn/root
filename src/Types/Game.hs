{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Game (
    -- Type
    Game,
    -- Lenses
    phaseStack,
    board,
    playerFactions,
    marquis,
    eerie,
    factionsInPlay,
    -- Constructors
    newForestGame,
) where

import Data.Maybe

import Lens.Micro
import Lens.Micro.TH

import Lookup.BoardLookup
import Types.Board
import Types.Default
import Types.Faction
import Types.Phase

----------------------------------
-- Type
----------------------------------
data Game = Game
    { _phaseStack :: [Phase]
    , _board :: Board
    , _playerFactions :: PlayerFactions
    }

data PlayerFactions = PlayerFactions
    { _marquis :: Maybe CatFaction
    , _eerie :: Maybe BirdFaction
    }

----------------------------------
-- Instances
----------------------------------
instance Default PlayerFactions where
    def = PlayerFactions Nothing Nothing

instance Default Game where
    def =
        Game
            { _phaseStack = []
            , _board = def
            , _playerFactions = def
            }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Game
makeLenses ''PlayerFactions

factionsInPlay :: SimpleGetter Game [Faction]
factionsInPlay = to factionsInPlay'
  where
    factionsInPlay' game =
        let cat = fmap (const Marquis) $ game ^. playerFactions . marquis
            bird = fmap (const Eerie) $ game ^. playerFactions . eerie
        in  catMaybes [cat, bird]

----------------------------------
-- Constructors
----------------------------------
newForestGame :: Game
newForestGame =
    def & (board .~ forestMap)
