{-# LANGUAGE TemplateHaskell #-}

module Types.Game (
    -- Type
    Game,
    newGame,
    -- Lenses
    units,
    cities,
    -- Stateful functions
    getUnit,
    deleteUnit,
    addUnit
) where

import Lens.Micro.TH
import Lens.Micro.Mtl

import Types.Alias
import Types.City
import Types.Error
import Types.IxTable as I
import Types.Unit
import Util

----------------------------------
-- Type
----------------------------------
data Game = Game
    { _cities :: [City]
    , _units :: IxTable Unit
    }
    deriving (Show)

----------------------------------
-- Constructor
----------------------------------
newGame :: Game
newGame = Game [] I.empty

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Game

----------------------------------
-- Stateful Functions
----------------------------------
getUnit :: UnitId -> Update Game Unit
getUnit uid = useEither LookupError $ units . atTable uid

deleteUnit :: UnitId -> Update Game ()
deleteUnit uid = units %= I.delete uid

addUnit :: Con Unit -> Update Game Unit
addUnit conUnit = do
    unitTable <- use units
    let (unitTable', unit) = I.insert conUnit unitTable
    units .= unitTable'
    return unit






