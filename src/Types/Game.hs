{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Game (
    -- Type
    Game,
    -- Constructor
    newGame,
    -- Lenses
    gameLog,
) where

import Lens.Micro.TH

import Types.LogEvent

----------------------------------
-- Type
----------------------------------
newtype Game = Game
    { _gameLog :: [LogEvent]
    }
    deriving (Show)

----------------------------------
-- Constructor
----------------------------------
newGame :: Game
newGame =
    Game
        { _gameLog = []
        }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Game

----------------------------------
-- Stateful Functions
----------------------------------
