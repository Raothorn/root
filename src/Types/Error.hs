module Types.Error (
    Error(..)
) where

data Error 
    = Error
    | UnitLookupError
    | MoveOutOfBoundsError
    | CityLookupError
    deriving (Show, Eq)

