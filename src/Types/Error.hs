module Types.Error (
    Error (..),
) where

data Error
    = Error
    | IndexError
    | EmptyPhaseStack
    | EmptyTypeEncountered
    | NoWarriorsInClearing
    | NotEnoughWarriors
    | NoPiecesInClearing
    | NotImplemented
    | WrongPhase
    | RecruitAlreadyUsed
    | WrongFaction
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
    | CannotMoveDueToRule
    deriving (Show, Eq)
