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
) where

import Lens.Micro.TH

import Types.Default
import Types.Faction
import Types.Phase
import Types.Board

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
            { _phaseStack = [def]
            , _board = def
            , _playerFactions = def
            }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Game
makeLenses ''PlayerFactions
