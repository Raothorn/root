{-# LANGUAGE TemplateHaskell #-}
module Types.Production (
    Production,
    ProductionType(..),
    -- Constructors
    newProduction,
    -- Lenses
    productionType,
    turnsRemaining
) where

import Lens.Micro.TH
----------------------------------
-- Types
----------------------------------
data Production = Production 
    { _productionType :: ProductionType
    , _turnsRemaining :: Int
    }
    deriving (Show)

data ProductionType 
    = UnitProduction
    deriving (Show)

----------------------------------
-- Constructors
----------------------------------
newProduction :: ProductionType -> Production
newProduction ptype = Production ptype 5

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Production
