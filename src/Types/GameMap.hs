module Types.GameMap (
    -- Types
    GameMap (..),
    -- Constructors
    -- Lenses
    -- Helpers
) where

import Lens.Micro

----------------------------------
-- Types
----------------------------------
data GameMap = GameMap 
    deriving (Show)

----------------------------------
-- Constructors
----------------------------------
newMap :: GameMap
newMap = GameMap

----------------------------------
-- Lenses
----------------------------------

----------------------------------
-- Helpers
----------------------------------
