module State.ClearingState (
    addToken,
    addWarrior,
    addBuilding,
    removeWarrior,
    removeWarriors,
    numFactionTokens,
    numFactionWarriors,
    numFactionBuildings,
    numFactionPieces,
    getOppositeCorner,
    getRulingFaction,
) where

import Control.Monad
import Data.Maybe
import qualified Data.List as List

import Lens.Micro
import Lens.Micro.Mtl

import Root.Types
import Types.Clearing
import qualified Types.Faction.FactionCommon as Com
import Util

----------------------------------
-- Updates
----------------------------------
addToken :: Token -> Update Clearing ()
addToken token = tokens %= (token :)

addWarrior :: Warrior -> Update Clearing ()
addWarrior warrior = warriors %= (warrior :)

addBuilding :: Building -> Update Clearing ()
addBuilding building = do
    numSlots <- use buildingSlots
    numBuildings <- use $ buildings . to length

    if numBuildings >= numSlots
        then liftErr NoFreeBuildingSlots
        else buildings %= (building :)

removeWarrior :: Faction -> Update Clearing (Maybe Warrior)
removeWarrior faction = do
    allWarriors <- use warriors

    let (factionWarriors, remainingWarriors) =
            List.partition ((== faction) . Com.warriorFaction) allWarriors

    case factionWarriors of
        (w:rm) -> do
            warriors .= rm ++ remainingWarriors
            return $ Just w
        _ -> return Nothing

removeWarriors :: Faction -> Int -> Update Clearing [Warrior]
removeWarriors faction num = do
    removed <- replicateM num $ removeWarrior faction
    if any isNothing removed
        then liftErr NotEnoughWarriors
        else return $ catMaybes removed

----------------------------------
-- Getters
----------------------------------

numFactionTokens :: Faction -> Update Clearing Int
numFactionTokens faction = do
    allTokens <- use tokens
    return $ length $ filter ((== faction) . Com.tokenFaction) allTokens

numFactionWarriors :: Faction -> Update Clearing Int
numFactionWarriors faction = do
    allWarriors <- use warriors
    return $ length $ filter ((== faction) . Com.warriorFaction) allWarriors

numFactionBuildings :: Faction -> Update Clearing Int
numFactionBuildings faction = do
    allBuildings <- use buildings
    return $ length $ filter ((== faction) . Com.buildingFaction) allBuildings

numFactionPieces :: Faction -> Update Clearing Int
numFactionPieces faction = do
    pieces <-
        sequence
            [ numFactionTokens faction
            , numFactionWarriors faction
            , numFactionBuildings faction
            ]
    return $ sum pieces

getOppositeCorner :: Update Clearing (Index Clearing)
getOppositeCorner = useMaybe NotCornerClearing oppositeCorner

{-
Rule 2.8: The ruler of a clearing is the player with the most total warriors and buildings
in that clearing. (Tokens and pawns do not contribute to rule.) If there is a tie between
players in a clearing, no one there is the ruler.
-}
getRulingFaction :: Update Clearing (Maybe Faction)
getRulingFaction = do
    let factions = [Marquis, Eerie]
    let numPieces faction =
            (+) <$> numFactionWarriors faction <*> numFactionBuildings faction

    pieces <- mapM numPieces factions
    let maxPieces = maximum pieces

    let winners = filter ((== maxPieces) . snd) $ zip factions pieces

    case winners of
        [(winner, _)] -> return $ Just winner
        _ -> return Nothing
