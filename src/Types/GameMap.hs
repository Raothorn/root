module Types.GameMap (
    -- Types
    GameMap (..),
    -- Constructors
    newMap,
    defaultMap,
    -- Lenses
    width,
    height,
    -- Helpers
    inBounds
) where

import Lens.Micro
import Data.Coerce (coerce)

import Types.Location

----------------------------------
-- Types
----------------------------------
newtype GameMap = GameMap (Int, Int)
    deriving (Show)

----------------------------------
-- Constructors
----------------------------------
newMap :: Int -> Int -> GameMap
newMap w h = GameMap (w, h)

defaultMap :: GameMap
defaultMap = newMap 30 15

----------------------------------
-- Lenses
----------------------------------
_gameMap :: Lens' GameMap (Int, Int)
_gameMap = lens coerce (\_ b -> GameMap b)

width :: Lens' GameMap Int
width = _gameMap . _1

height :: Lens' GameMap Int
height = _gameMap . _2

----------------------------------
-- Helpers
----------------------------------
inBounds :: Location -> GameMap -> Bool
inBounds l gm = 
    l ^. x >= 0 &&
    l ^. y >= 0 && 
    l ^. x < gm ^. width &&
    l ^. y < gm ^. height
