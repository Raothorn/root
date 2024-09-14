module Types.Location (
    Location(..),
    Direction(..)
) where

newtype Location = Location (Int, Int)

data Direction = N | S | E | W
