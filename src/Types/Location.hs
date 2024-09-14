module Types.Location (
    Location(..),
    Direction(..),
    x, 
    y,
) where

import Lens.Micro

newtype Location = Location { unLocation :: (Int, Int) }

data Direction = N | S | E | W

x :: Lens' Location Int
x = location . _1

y :: Lens' Location Int 
y = location . _2

location :: Lens' Location (Int, Int)
location = lens unLocation (\_ b -> Location b)
