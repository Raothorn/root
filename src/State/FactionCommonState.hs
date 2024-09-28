module State.FactionCommonState (
    addCard,
    craftCard,
    removeCard,
    removeWarrior,
) where

import Lens.Micro
import Lens.Micro.Mtl

import Root.Types
import qualified Types.Card as Card
import Types.Faction.FactionCommon
import Util

----------------------------------
-- Stateful functions
----------------------------------
addCard :: Index Card -> Update FactionCommon ()
addCard cardIx = do
    hand %= (cardIx :)
    fac <- use faction
    logEvent $ CardGained cardIx fac

{-
Parameters:
    card :: Card - the card the faction is crafting
Errors: EmptyTypeEncountered if the card effect is NoEffect
Updates: The victory points from the card are added to the faction VPs
Returns: Nothing
-}
craftCard :: Card -> Update FactionCommon ()
craftCard card = do
    -- Log the crafting event
    fac <- use faction
    logEvent $ CardCrafted (getIx card) fac

    case card ^. Card.effect of
        VictoryPoints n -> do
            victoryPoints += n
            logEvent $ GainedVictoryPoints n
        NoEffect -> liftErr EmptyTypeEncountered

{-
Parameters:
    `cardIx :: Index Card` - the index of the card to remove
Errors: `CardNotInHand` if the card is not present in `hand`
Updates: The card is no longer in the hand
-}
removeCard :: Index Card -> Update FactionCommon ()
removeCard cardIx = do
    curHand <- use hand
    if cardIx `elem` curHand
        then hand %= filter (/= cardIx)
        else liftErr CardNotInHand

{-
Parameters: None
Updates: One warrior is removed from the supply, if there are any
Returns: A list containing a single warrior, or an empty list if the supply is empty
-}
removeWarrior :: Update FactionCommon (Maybe Warrior)
removeWarrior = do
    numWarriors <- use warriors
    fac <- use faction
    if numWarriors > 0
        then do
            warriors -= 1
            return $ Just (getWarrior fac)
        else return Nothing
  where
    getWarrior fac =
        case fac of
            Marquis -> CatWarrior
            Eerie -> BirdWarrior
