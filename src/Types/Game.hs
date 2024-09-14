{-# LANGUAGE TemplateHaskell #-}
module Types.Game (
    -- Type
    Game,
    -- Lenses
    unit,
) where

import Lens.Micro.TH

import Types.Unit

data Game = Game {
    _unit :: Unit,
    _dummy :: Int
}

makeLenses ''Game
