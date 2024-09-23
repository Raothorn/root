module Types.Error (
    Error(..)
) where

data Error 
    = Error
    | EmptyTypeEncountered
    | NotImplemented
    | NotFactionTurn
    | CannotAffordCraft
    | CardNotInHand
    deriving (Show, Eq)
