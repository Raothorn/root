{-# LANGUAGE TemplateHaskell #-}

module Types.Unit (
    Unit,
    UnitAction (..),
    UnitClass (..),
    UnitId,
    -- Constructors
    newUnit,
    -- Helpers
    availableActions,
    actionAvailable,
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
type UnitId = Int

data UnitClass = Settler
    deriving (Show)

data UnitAction
    = BuildCity
    deriving (Eq)

data Unit = Unit
    { _unitId :: UnitId
    , _unitClass :: UnitClass
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
    case unit ^. unitClass of
        Settler -> [BuildCity]

actionAvailable :: Unit -> UnitAction -> Bool
actionAvailable unit action = action `elem` actions
  where
    actions = availableActions unit

----------------------------------
-- Aliases
----------------------------------
