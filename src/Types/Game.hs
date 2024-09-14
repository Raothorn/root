{-# LANGUAGE TemplateHaskell #-}
module Types.Game (
    -- Type
    Game,
    newGame,
    -- Lenses
    unit,
) where

import Lens.Micro.TH

import Types.Location
import Types.Unit

data Game = Game {
    _unit :: Unit
}

newGame :: Game 
newGame = Game { _unit = Location (0, 0) }

makeLenses ''Game
