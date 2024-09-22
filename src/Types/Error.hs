module Types.Error (
    Error(..)
) where

data Error 
    = Error
    | NotImplemented
    | NotFactionTurn
    deriving (Show, Eq)

