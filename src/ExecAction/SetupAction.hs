module ExecAction.SetupAction (
    execSetupAction
) where

import Control.Monad

import Lens.Micro
import Lens.Micro.Mtl

import Root.Types
import qualified Root.Game as Game
import qualified Root.Board as Board
import qualified Root.Marquis as Cat
import qualified Root.Clearing as Clr
import qualified Root.FactionCommon as Com
import Types.Default
import Types.IxTable
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

    -- Verify that the chosen keep clearing is a corner clearing
    keepClearing <-
        useMaybe IndexError $
            Game.board . Board.clearings . atTable (setup ^. Cat.keepCorner)

    -- If oppositeCorner == Nothing, it is not a corner clearing
    oppositeClearing <- liftMaybe NotCornerClearing (keepClearing ^. Clr.oppositeCorner)

    -- Place the keep token in the chosen clearing
    Game.zoomClearing' keepClearing $ Clr.addToken Keep

    -- Place a warrior in each clearing except the opposite one
    allClearings <- use $ Game.board . Board.clearings . to values
    forM_ allClearings $ \clearing -> do
        when (getIx clearing /= oppositeClearing) $ do
            warrior <- Game.zoomCat $ zoom Cat.common Com.removeWarrior
            forM_ warrior $ \w ->
                Game.zoomClearing' clearing $ Clr.addWarrior w

    -- Place starting buildings
    let buildings =
            [ (Sawmill, setup ^. Cat.sawmillClearing)
            , (Workshop, setup ^. Cat.workshopClearing)
            , (Recruiter, setup ^. Cat.recruiterClearing)
            ]

    forM_ buildings $ \(building, clearingIx) -> do
        -- Verify the chosen clearing is the keep clearing or adjacent to it
        let isValid = clearingIx == getIx keepClearing || Clr.isAdjacent keepClearing clearingIx
        unless isValid $ liftErr InvalidBuildingLocation

        -- Place the building in the clearing
        Game.zoomClearing clearingIx $ Clr.addBuilding building

execSetupAction _ _ = liftErr NotImplemented
