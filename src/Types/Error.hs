module Types.Error (
    Error(..)
) where

data Error 
    = Error
    | LookupError
    deriving (Show)

