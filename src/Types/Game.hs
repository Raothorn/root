{-# LANGUAGE TemplateHaskell #-}

module Types.Game (
    -- Type
    Game,
    newGame,
    -- Lenses
    unit,
    cities,
) where

import Lens.Micro.TH

import Types.City
import Types.Unit

data Game = Game
    { _cities :: [City]
    , _unit :: Unit
    }

newGame :: Game
newGame = Game [] newUnit

makeLenses ''Game
