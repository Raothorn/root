module Util.MapUtil (
    adjacentLocation
) where

import Types

adjacentLocation :: Direction -> Location -> Location
adjacentLocation N (Location (x, y)) = Location (x, y - 1)
adjacentLocation S (Location (x, y)) = Location (x, y + 1)
adjacentLocation W (Location (x, y)) = Location (x - 1, y)
adjacentLocation E (Location (x, y)) = Location (x + 1, y)
