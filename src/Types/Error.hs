module Types.Error (
    Error(..)
) where

data Error 
    = Error
    | EmptyTypeEncountered
    | NotImplemented
    | WrongPhase
    | NoActionsRemaining
    | NotFactionTurn
    | CannotAffordCraft
    | CardNotInHand
    deriving (Show, Eq)
