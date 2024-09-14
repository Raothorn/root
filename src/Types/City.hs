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

----------------------------------
-- Types
----------------------------------
type Production = ()

type CityId = Int

data City = City
    { _cityId :: CityId
    , _location :: Location
    , _productionQueue :: [Production]
    }
    deriving (Show)
----------------------------------
-- Lenses
----------------------------------`
makeLenses ''City

----------------------------------
-- Constructors
----------------------------------
newCity :: Location -> Con City
newCity l cid = City cid l []
