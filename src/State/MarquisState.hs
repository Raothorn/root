{-# HLINT ignore "Redundant <$>" #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module State.MarquisState (
    takeWoodToken,
    addWoodToken,
    removeBuilding,
    buildingCost,
) where

import Control.Monad

import Lens.Micro
import Lens.Micro.Mtl

import Root.Types
import Types.Faction.Marquis
import Util

----------------------------------
-- Stateful functions
----------------------------------
addWoodToken :: Update CatFaction ()
addWoodToken = woodTokens += 1

{-
Parameters: none
Errors: none
Returns: either an empty list or a list containing a single wood token.
-}
takeWoodToken :: Update CatFaction [Token]
takeWoodToken = do
    remainingWood <- use woodTokens
    if remainingWood > 0
        then do
            woodTokens -= 1
            return [Wood]
        else return []

removeBuilding :: Building -> Update CatFaction (Int, Int)
removeBuilding building = do
    numRemaining <- use $ buildings building
    when (numRemaining == 0) $ liftErr NoBuildingsRemaining

    -- remove the building from the track
    buildings building -= 1

    vps <- buildingVps building
    -- TODO calculate the number of cards to draw after building a building
    draws <- buildingDraws building

    return (vps, draws)

-- Calculates the building cost based on the remaining buildings on the track.
buildingCost :: Building -> Update CatFaction Int
buildingCost building = do
    remaining <- use $ buildings building
    buildingCost' remaining

buildingCost' :: Int -> Update CatFaction Int
buildingCost' remaining
    | remaining > 6 || remaining < 0 = liftErr WrongAmount
    | otherwise = return (6 - remaining)

buildingVps :: Building -> Update CatFaction Int
buildingVps Sawmill = sawmillVps <$> use sawmills >>= liftMaybe Error
buildingVps Workshop = workshopVps <$> use workshops >>= liftMaybe Error
buildingVps Recruiter = recruiterVps <$> use recruiters >>= liftMaybe Error

sawmillVps :: Int -> Maybe Int
sawmillVps = flip lookup [(6, 0), (5, 1), (4, 2), (3, 3), (2, 4), (1, 5)]

workshopVps :: Int -> Maybe Int
workshopVps = flip lookup [(6, 0), (5, 2), (4, 2), (3, 3), (2, 4), (1, 5)]

recruiterVps :: Int -> Maybe Int
recruiterVps = flip lookup [(6, 0), (5, 1), (4, 2), (3, 3), (2, 3), (1, 4)]

buildingDraws :: Building -> Update CatFaction Int
buildingDraws Sawmill = return 0
buildingDraws Workshop = return 0
buildingDraws Recruiter = do
    remaining <- use recruiters
    case remaining of
        2 -> return 1
        4 -> return 1
        _ -> return 0
