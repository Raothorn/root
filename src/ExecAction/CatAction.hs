module ExecAction.CatAction (
    execCatAction,
) where

import Control.Monad

import Lens.Micro
import Lens.Micro.Mtl

import qualified Root.Board as Board
import qualified Root.Card as Card
import qualified Root.Clearing as Clr
import qualified Root.FactionCommon as Com
import qualified Root.Game as Game
import Root.Lookup
import qualified Root.Marquis as Cat
import Root.Types
import Types.Default
import Types.IxTable
import Util

----------------------------------
-- Marquis Actions
----------------------------------
execCatAction :: CatPhase -> CatAction -> Update Game ()
----------------------------------
-- Setup
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

execCatAction CatSetupPhase (CatFactionSetup setup) = do
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

----------------------------------
-- Birdsong
----------------------------------
{- Rule 6.4: Place one wood token at each sawmill -}
-- TODO: if there are not enough wood tokens in the supply,
-- allow the player to decide where they go. Temporarily,
-- we just allocate all the tokens arbitrarily.
----------------------------------
-- Place Wood
----------------------------------
execCatAction CatPlaceWoodPhase CatPlaceWood = do
    -- Get all the clearings with a sawmill
    sawmillClearings <-
        zoom Game.board $
            Board.getClearingsP (Clr.hasBuilding Sawmill)

    forM_ sawmillClearings $ \clearing -> do
        -- Take a wood token, if possible
        woodToken <- Game.zoomCat Cat.takeWoodToken

        -- Place the wood token in the clearing
        forM_ woodToken $ \w -> do
            Game.zoomClearing' clearing (Clr.addToken w)
    -- Game.logEvent $ WoodPlaced (getIx clearing)

    -- Advance the phase
    Game.setPhase $ MarquisPhase $ CatCraftPhase []

----------------------------------
-- Daylight
----------------------------------
{-
Rule 6.5: First, you may activate workshops to craft cards from your hand.
Then, you may take up to three actions in any order and combination. After
taking three actions, you may take extra actions by spending one bird card
per extra action.
-}
----------------------------------
-- Craft
----------------------------------
execCatAction (CatCraftPhase workshopsUsed) (CatCraft cardIx) = do
    -- Calculate how many suits the Marquis has available to spend

    -- Get the suits of all the clearings with a workshop
    workshopSuits <-
        zoom Game.board $ do
            clearings <- Board.getClearingsP (Clr.hasBuilding Workshop)
            return $ map (^. Clr.suit) clearings

    -- Subtract the already used workshops
    let craftingSuits = bagDifference workshopSuits workshopsUsed

    -- Get the cost of the card
    let cardCost = lookupCard cardIx ^. Card.craftCost

    -- Ensure the Marquis has enough remaining workshops to craft the card
    if bagSubsetOf cardCost craftingSuits
        then do
            Game.zoomCat $ zoom Cat.common $ do
                -- Remove the card from the Marquis' hand
                card <- Com.removeCard cardIx
                -- Craft the card
                Com.craftCard card

            -- Update "workshopsUsed"
            Game.setPhase $ MarquisPhase $ CatCraftPhase (workshopsUsed ++ cardCost)
        else liftErr CannotAffordCraft

----------------------------------
-- Daylight Actions
----------------------------------
execCatAction (CatChooseActionPhase n) action = do
    when (n <= 0) $ liftErr NoActionsRemaining
    -- We defer checking if the action is valid to the dispatched function
    execDaylightAction action

----------------------------------
-- Otherwise
----------------------------------
execCatAction _ _ = liftErr NotImplemented

execDaylightAction :: CatAction -> Update Game ()
execDaylightAction CatBattle = liftErr NotImplemented
execDaylightAction CatMarch = liftErr NotImplemented
execDaylightAction CatRecruit = liftErr NotImplemented
execDaylightAction CatBuild = liftErr NotImplemented
execDaylightAction CatOverwork = liftErr NotImplemented
execDaylightAction _ = liftErr WrongPhase
