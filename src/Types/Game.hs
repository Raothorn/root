{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Types.Game (
    -- Type
    Game,
    newGame,
    -- Lenses
    units,
    cities,
    -- Stateful functions
    getUnit,
    getCity,
    inMapBounds,
    addIxEntry,
    deleteIxEntry
) where

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Types.Alias
import Types.City
import Types.Error
import Types.GameMap
import Types.IxTable as I
import qualified Types.Location as L
import Types.Unit
import Util

----------------------------------
-- Type
----------------------------------
data Game = Game
    { _cities :: IxTable City
    , _units :: IxTable Unit
    , _gameMap :: GameMap
    }
    deriving (Show)

----------------------------------
-- Constructor
----------------------------------
newGame :: Game
newGame = Game I.empty I.empty defaultMap

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Game

----------------------------------
-- Stateful Functions
----------------------------------
----------------------------------
-- Unit
----------------------------------
-- We specifically provide a getter so that client code doesn't
-- decide what error to use
getUnit :: UnitId -> Update Game Unit
getUnit = getIxEntry UnitLookupError units
----------------------------------
-- Map
----------------------------------
inMapBounds :: L.Location -> Update Game Bool
inMapBounds l = do
    gm <- use gameMap
    return $ inBounds l gm

----------------------------------
-- City
----------------------------------
getCity :: CityId -> Update Game City
getCity = getIxEntry CityLookupError cities
----------------------------------
-- General
----------------------------------
addIxEntry :: Lens' Game (IxTable a) -> Con a -> Update Game a
addIxEntry l con = do
    table <- use l
    let (table', x) = I.insert con table
    l .= table'
    return x

getIxEntry :: Error -> Lens' Game (IxTable a) -> Int -> Update Game a
getIxEntry err l entryId = useEither err $ l . atTable entryId

deleteIxEntry :: Lens' Game (IxTable a) -> Int -> Update Game ()
deleteIxEntry l entryId = l %= I.delete entryId
