{-# LANGUAGE TemplateHaskell #-}
module Types.City (
    City,
    CityId,
    --Lenses
    cityId,
    location,
    productionQueue, 
    -- Constructors
    newCity,
) where

import Lens.Micro.TH

import Types.Location
import Types.Alias
import qualified Types.IxTable as I
import Data.Coerce (coerce)

----------------------------------
-- Types
----------------------------------
newtype CityId = CityId Int
    deriving (Show)

type Production = ()

data City = City
    { _cityId :: CityId
    , _location :: Location
    , _productionQueue :: [Production]
    }
    deriving (Show)

----------------------------------
-- Instances
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
newCity l cid = City cid l []
