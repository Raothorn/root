module Types.Error (
    Error (..),
) where

data Error
    = Error
    | IndexError
    | EmptyPhaseStack
    | WrongAmount
    | EmptyTypeEncountered
    | NoWarriorsInClearing
    | NotEnoughWarriors
    | NoBuildingsRemaining
    | SuitDoesNotMatch
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
    | NotEnoughTokens
    | ClearingsNotConnectedByRule
    | NoFreeBuildingSlots
    | EmptySupply
    | InvalidBuildingLocation
    | TraversalError
    | CannotMoveDueToRule
    deriving (Show, Eq)
