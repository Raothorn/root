module ExecAction.SetupAction (
    execSetupAction,
) where

import Control.Monad
import Data.List ((\\))

import Lens.Micro
import Lens.Micro.Mtl

import qualified Root.Clearing as Clr
import qualified Root.Game as Game
import qualified Root.Marquis as Cat
import Root.Types
import Util

execSetupAction :: FactionSetupPhase -> SetupAction -> Update Game ()
----------------------------------
-- Marquis Setup
----------------------------------
{- Rule 6.3: Faction setup
Step 1: Gather Warriors and Wood. Form supplies of 25 warriors and 8 wood tokens near you.

Step 2: Place Keep. Place the keep token in the corner clearing of your choice.

Step 3: Garrison. Place a warrior in each clearing except the clearing in the diagonally
opposite corner from the clearing with the keep token.

Step 4: Place Starting Buildings. Place 1 sawmill, 1 workshop, and 1 recruiter. You may
place them among the clearing with the keep token and any adjacent clearings, in any combination.

Step 5: Fill Buildings Tracks. Place your remaining 5 sawmills, 5 workshops, and 5 recruiters
on your matching Buildings tracks, filling every space except the leftmost space of each track.
-}

execSetupAction CatSetupPhase (CatSetupAction setup) = do
    -- Initilize the faction (steps 1 and 5)
    Game.playerFactions . Game.marquis ?= def

    -- Get the opposite corner of the keep clearing
    let keepCorner = setup ^. Cat.keepCorner
    -- This will error if it is not a corner clearing
    oppositeCorner <- zoomT (Game.clearingAt keepCorner) Clr.getOppositeCorner

    -- Place the keep token in the chosen clearing
    zoom (Game.clearingAt keepCorner) $ Clr.addToken Keep

    -- Place a warrior in each clearing except the opposite one
    allClearings <- use Game.allClearingIxs
    let warriorClearings = allClearings \\ [oppositeCorner]
    forM_ warriorClearings $ \clearing -> Game.placeWarrior Marquis clearing

    -- Place starting buildings
    let buildings =
            [ (Sawmill, setup ^. Cat.sawmillClearing)
            , (Workshop, setup ^. Cat.workshopClearing)
            , (Recruiter, setup ^. Cat.recruiterClearing)
            ]
    forM_ buildings $ \(building, clearingIx) -> do
        zoomT (Game.clearingAt clearingIx) $ do
            -- Check that the building can be placed in the clearing
            isAdjacent <- Clr.isAdjacent keepCorner
            unless (clearingIx == keepCorner || isAdjacent) $ liftErr InvalidBuildingLocation
            -- Place the building in the clearing
            Clr.addBuilding building
execSetupAction _ _ = liftErr NotImplemented
