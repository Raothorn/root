module Types.Location (
    Location(..),
    Direction(..),
    -- lenses
    x, 
    y,
    location,
    -- constructors
    origin,
) where

import Lens.Micro

----------------------------------
-- Types
----------------------------------
newtype Location = Location { unLocation :: (Int, Int) }
    deriving (Eq, Show)

data Direction = N | S | E | W

----------------------------------
-- Lenses
----------------------------------

x :: Lens' Location Int
x = location . _1

y :: Lens' Location Int 
y = location . _2

location :: Lens' Location (Int, Int)
location = lens unLocation (\_ b -> Location b)

----------------------------------
-- Constructors
----------------------------------
origin :: Location 
origin = Location (0, 0)

