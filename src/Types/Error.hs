module Types.Error (
    Error (..),
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
    | FactionNotInPlay
    | CannotAffordCraft
    | CardNotInHand
    | NotCornerClearing
    | NoFreeBuildingSlots
    | EmptySupply
    | InvalidBuildingLocation
    | TraversalError
    deriving (Show, Eq)
