module Types.Error (
    Error(..)
) where

data Error 
    = Error
    | IndexError
    | EmptyPhaseStack
    | EmptyTypeEncountered
    | NotImplemented
    | WrongPhase
    | NoActionsRemaining
    | NotFactionTurn
    | CannotAffordCraft
    | CardNotInHand
    | NotCornerClearing
    | NoFreeBuildingSlots
    | EmptySupply
    | InvalidBuildingLocation
    deriving (Show, Eq)
