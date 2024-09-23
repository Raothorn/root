{-# LANGUAGE TemplateHaskell #-}

module Types.Faction.FactionCommon (
    -- Types
    Faction(..),
    FactionCommon,
    -- Lenses
    hand,
    -- Stateful functions
    craftCard,
    removeCard,
) where

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Lookup.CardLookup (lookupCard)
import Types.Alias
import Types.Card (Card, CardEffect (..))
import qualified Types.Card as Card
import Types.Default
import Types.Error
import Types.IxTable
import Util

----------------------------------
-- Types
----------------------------------
data Faction
    = Marquis
    | Eerie
    | NoFaction

data FactionCommon = FactionCommon
    { _faction :: Faction
    , _hand :: [Index Card]
    , _victoryPoints :: Int
    }

----------------------------------
-- Instances
----------------------------------
instance Default FactionCommon where
    def =
        FactionCommon
            { _faction = NoFaction
            , _hand = []
            , _victoryPoints = 0
            }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''FactionCommon

----------------------------------
-- Stateful functions
----------------------------------
{-
Parameters: card :: Card - the card the faction is crafting
Errors: EmptyTypeEncountered if the card effect is NoEffect
Updates: The victory points from the card are added to the faction VPs
Returns: Logs
-}
craftCard :: Card -> Update FactionCommon ()
craftCard card = do
    case card ^. Card.effect of
        VictoryPoints n -> victoryPoints += n
        NoEffect -> liftErr EmptyTypeEncountered
{-
Parameters: `cardIx :: Index Card` - the index of the card to remove
Errors: `CardNotInHand` if the card is not present in `hand`
Updates: The card is no longer in the hand
Returns: The card data associated with the card index
-}
removeCard :: Index Card -> Update FactionCommon Card
removeCard cardIx = do
    curHand <- use hand
    if cardIx `elem` curHand
        then do
            hand %= filter (/= cardIx)
            return $ lookupCard cardIx
        else liftErr CardNotInHand
