module Types.Location (
    Location(..),
    Direction(..),
    -- lenses
    x, 
    y,
    -- constructors
    origin,
    adjacentLocation
) where

import Lens.Micro
import Data.Coerce (coerce)

----------------------------------
-- Types
----------------------------------
newtype Location = Location (Int, Int)
    deriving (Eq, Show)

data Direction = N | S | E | W
    deriving (Show)

----------------------------------
-- Lenses
----------------------------------

x :: Lens' Location Int
x = _location . _1

y :: Lens' Location Int 
y = _location . _2

_location :: Lens' Location (Int, Int)
_location = lens coerce (\_ b -> Location b)
----------------------------------
-- Constructors
----------------------------------
origin :: Location 
origin = Location (0, 0)

adjacentLocation :: Direction -> Location -> Location
adjacentLocation N = y -~ 1
adjacentLocation S = y +~ 1
adjacentLocation W = x -~ 1
adjacentLocation E = x +~ 1
