{-# LANGUAGE TemplateHaskell #-}
module Types.Unit (
    Unit,
    UnitAction(..),
    UnitClass(..),
    UnitId,
    -- Constructors
    newUnit,
    -- Helpers
    availableActions,
    -- Lenses
    location,
    unitId,
) where

import Lens.Micro
import Lens.Micro.TH

import Types.Alias
import qualified Types.Location as L

----------------------------------
-- Types
----------------------------------
data UnitClass = Settler
    deriving (Show)

data UnitAction
    = BuildCity

data Unit = Unit
    { _unitId:: Int
    , _unitType :: UnitClass
    , _location :: L.Location
    }
    deriving (Show)

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Unit

----------------------------------
-- Constructors
----------------------------------
newUnit ::
    UnitClass ->
    L.Location ->
    Con Unit
newUnit utype uloc uid = Unit uid utype uloc

----------------------------------
-- Helpers
----------------------------------
-- TODO refactor as "actionAvaialabe Unit -> UnitAction -> Bool"
availableActions :: Unit -> [UnitAction]
availableActions unit =
    case unit ^. unitType of
        Settler -> [BuildCity]

----------------------------------
-- Aliases
----------------------------------
type UnitId = Int
