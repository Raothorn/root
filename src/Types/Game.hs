{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use <$>" #-}

module Types.Game (
    -- Type
    Game,
    newGame,
    -- Lenses
    units,
    cities,
    -- Stateful functions
    advanceTurn,
    getUnit,
    getIdleUnit,
    getCity,
    inMapBounds,
    addIxEntry,
    deleteIxEntry
) where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Maybe

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Types.Alias
import Types.City as C
import Types.Error
import Types.GameMap
import Types.IxTable as I
import qualified Types.Location as L
import Types.Unit as U
import Util
import Types.Production

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
advanceTurn :: Update Game ()
advanceTurn = do
    productions <- zoom (cities.traversed) $ do
        loc <- use C.location
        ptype <- updateProductionQueue 
        let result = fmap (\p -> (loc, p)) ptype
        return $ maybeToList result

    forM_ productions $ uncurry produce

produce :: L.Location -> ProductionType -> Update Game ()
produce loc ptype = do
    case ptype of
        UnitProduction -> do
            let unitCon = newUnit Settler loc
            void $ addIxEntry units unitCon

----------------------------------
-- Unit
----------------------------------
-- We specifically provide a getter so that client code doesn't
-- decide what error to use
getUnit :: UnitId -> Update Game Unit
getUnit = getIxEntry UnitLookupError units

getIdleUnit :: Update Game (Maybe UnitId)
getIdleUnit = preuse $ units . to values . _head . U.unitId

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
addIxEntry :: (Id i) => Lens' Game (IxTable a) -> Con i a -> Update Game a
addIxEntry l con = do
    table <- use l
    let (table', x) = I.insert con table
    l .= table'
    return x

getIxEntry :: (Id i) =>  Error -> Lens' Game (IxTable a) -> i -> Update Game a
getIxEntry err l entryId = useEither err $ l . atTable entryId

deleteIxEntry :: (Id i) => Lens' Game (IxTable a) -> i -> Update Game ()
deleteIxEntry l entryId = l %= I.delete entryId
