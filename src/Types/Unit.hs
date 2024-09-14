{-# LANGUAGE TemplateHaskell #-}
module Types.Unit (
    Unit,
    UnitAction(..),
    newUnit,
    -- Lenses
    location,
) where

import Lens.Micro.TH

import Types.Location

----------------------------------
-- Types
----------------------------------
data UnitClass = Settler

data UnitAction 
    = BuildCity

data Unit = Unit
    { _unitClass :: UnitClass
    , _location :: Location
    }

----------------------------------
-- Constructors
----------------------------------
newUnit :: Unit
newUnit = Unit Settler (Location (0, 0))

----------------------------------
-- Helpers
----------------------------------
unitActions :: UnitClass -> [UnitAction]
unitActions Settler = [BuildCity]

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Unit
