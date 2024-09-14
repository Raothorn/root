{-# LANGUAGE TemplateHaskell #-}

module Types.City (
    -- Types
    City,
    CityId,
    -- Lenses
    cityId,
    location,
    productionQueue,
    -- Constructors
    newCity,
    -- Stateful functions
    updateProductionQueue,
    queueProduction,
    getTurnsRemaining
) where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Coerce (coerce)
import Data.Maybe 

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Types.Alias
import qualified Types.IxTable as I
import Types.Location
import Types.Production as P

----------------------------------
-- Types
----------------------------------
newtype CityId = CityId Int
    deriving (Show)

data City = City
    { _cityId :: CityId
    , _location :: Location
    , _productionQueue :: Maybe Production
    }
    deriving (Show)

---------------------------------- Instances
----------------------------------
instance I.Id CityId where
    toInt = coerce
    fromInt = CityId

----------------------------------
-- Lenses
----------------------------------`
makeLenses ''City

----------------------------------
-- Constructors
----------------------------------
newCity :: Location -> Con CityId City
newCity l cid = City cid l Nothing

----------------------------------
-- Stateful functions
----------------------------------
updateProductionQueue :: Update City (Maybe ProductionType)
updateProductionQueue = do
    -- Get the production if it's finished, otherwise 'Nothing'
    result <- zoom (productionQueue . traversed) $ do
        turns' <- P.turnsRemaining <%= subtract 1
        if turns' == 0
            then do
                ptype <- use P.productionType
                return [ptype]
            else return []

    -- If the production is finished, remove it
    unless (null result) $ productionQueue .= Nothing
    return (listToMaybe result)

queueProduction :: ProductionType -> Update City ()
queueProduction ptype = do
    queue <- use productionQueue
    when (isNothing queue) $ do
        let production = P.newProduction ptype
        productionQueue ?= production

getTurnsRemaining :: Update City (Maybe Int)
getTurnsRemaining = do
    queue <- use productionQueue
    case queue of  
        Just p -> return $ Just (p ^. P.turnsRemaining)
        _ -> return Nothing

