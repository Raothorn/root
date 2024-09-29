{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ExecAction.CatAction (
    execCatAction,
) where

import Control.Monad
import Control.Monad.Trans.State.Lazy

import Lens.Micro
import Lens.Micro.Mtl

import qualified Root.Card as Card
import qualified Root.Clearing as Clr
import qualified Root.FactionCommon as Com
import qualified Root.Game as Game
import qualified Root.Marquis as Cat
import Root.Types
import Util

----------------------------------
-- Marquis Actions
----------------------------------
execCatAction :: CatPhase -> CatAction -> Update Game ()
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
    sawmillClearings <- Game.getClearingsWhere (Clr.hasBuilding Sawmill)

    forM_ sawmillClearings $ \clearing -> do
        -- Take a wood token, if possible
        woodToken <- zoom Game.catFaction Cat.takeWoodToken

        -- Place the wood token in the clearing
        forM_ woodToken $ \w -> do
            zoom (Game.clearingAt clearing) $ do
                Clr.addToken w
                logEvent $ WoodPlaced clearing

    -- Advance the phase
    Game.setPhase $ FactionTurnPhase $ MarquisPhase $ CatCraftPhase []

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
    workshopSuits <- zoom Game.allClearings $ do
        hasWorkshop <- gets $ Clr.hasBuilding Workshop
        if hasWorkshop
            then pure <$> use Clr.suit
            else return []

    -- Subtract the already used workshops
    let craftingSuits = bagDifference workshopSuits workshopsUsed

    -- Get the cost of the card
    card <- Game.lookupCard cardIx
    let cardCost = card ^. Card.craftCost

    -- Ensure the Marquis has enough remaining workshops to craft the card
    if bagSubsetOf cardCost craftingSuits
        then do
            zoom (Game.factionCommon Marquis) $ do
                -- Remove the card from the Marquis' hand
                Com.removeCard cardIx
                -- Craft the card
                Com.craftCard card

            -- Update "workshopsUsed"
            Game.setPhase $
                FactionTurnPhase $
                    MarquisPhase $
                        CatCraftPhase (workshopsUsed ++ cardCost)
        else liftErr CannotAffordCraft
execCatAction (CatCraftPhase _) CatFinishCrafting = do
    -- Advance the phase
    Game.setPhase $ FactionTurnPhase $ MarquisPhase $ CatDaylightActionPhase 3 False
----------------------------------
-- Daylight Actions
----------------------------------
execCatAction (CatDaylightActionPhase n hasRecruited) action = do
    when (n <= 0) $ liftErr NoActionsRemaining
    -- We defer checking if the action is valid to the dispatched function
    recruiting <- case action of
        CatRecruit -> do
            when hasRecruited $ liftErr RecruitAlreadyUsed
            return True
        _ -> return False

    execDaylightAction action
    Game.setPhase $
        FactionTurnPhase $
            MarquisPhase $
                CatDaylightActionPhase (n - 1) recruiting

----------------------------------
-- Otherwise
----------------------------------
execCatAction _ _ = liftErr WrongPhase

execDaylightAction :: CatAction -> Update Game ()
----------------------------------
-- Battle
----------------------------------
execDaylightAction (CatBattle clearing defender) = do
    Game.initiateBattle Marquis defender clearing
----------------------------------
-- March
----------------------------------
execDaylightAction (CatMarch fromIx toIx numWarriors) = do
    Game.moveWarriors Marquis numWarriors fromIx toIx
----------------------------------
-- Recruit
----------------------------------
{-
Recruit. Place one warrior at each recruiter. You may take this action only once per turn.
-}
execDaylightAction CatRecruit = do
    -- Get all the clearings with a recruiter
    recruiterClearings <- Game.getClearingsWhere (Clr.hasBuilding Recruiter)

    forM_ recruiterClearings $ \clearing ->
        Game.takeWarriorFromSupplyAndPlace Marquis clearing
----------------------------------
-- Build
----------------------------------`
{-
Build. Place a building.
Choose Building. Choose whether you want to place a sawmill, workshop, or recruiter. Find
the leftmost building of that type remaining on your faction board. Note that building’s cost,
listed at the top of its column.
Choose Clearing and Pay Wood. Choose any clearing you rule. Remove wood tokens equal in number
to the building’s cost from the chosen clearing, any adjacent clearings you rule, or any
clearings connected to the chosen clearing you rule through any number of clearings you rule.
Place Building and Score. Place the chosen building on the chosen clearing, and score the
victory points revealed on the space under the building removed from your faction board.
-}
execDaylightAction (CatBuild clearing building woodClearings) = do
    allWoodTokens <- forM woodClearings $ \(woodClearing, numTokens) -> do
        -- Verify that the clearing is connected by a path of ruled clearings
        connected <- Game.areClearingsConnected Marquis clearing woodClearing []
        unless connected $ liftErr ClearingsNotConnectedByRule

        -- Will propogate an error if there are not enough wood tokens
        zoomT (Game.clearingAt woodClearing) $ 
            Clr.removeTokens Wood numTokens
    let woodTokens = concat allWoodTokens

    -- Verify that the Marquis has selected enough wood tokens
    buildingCost <- zoomT Game.catFaction $ Cat.buildingCost building
    unless (length woodTokens == buildingCost) $
        liftErr WrongAmount

    -- Remove the building from the faction board
    (buildingVps, draw) <- zoomT Game.catFaction $ Cat.removeBuilding building

    -- Place the building
    zoom (Game.clearingAt clearing) $ Clr.addBuilding building

    -- Score the victory points
    Game.factionCommon Marquis . Com.victoryPoints += buildingVps

    -- Draw cards
    -- replicateM_ draw $ do
        -- Draw a card
        -- cardIx <- Game.drawCard
        -- Give the card to the Marquis
        -- Game.giveCard Marquis cardIx


----------------------------------
-- Overwork
----------------------------------
{-
Overwork. Spend a card matching the clearing of a sawmill, and place a wood token there.
-}
execDaylightAction (CatOverwork cardIx clearing) = do
    -- Ensure the card matches the clearing
    card <- Game.lookupCard cardIx
    let cardSuit = card ^. Card.suit

    clearingSuit <- liftTraversal IndexError $ Game.clearingAt clearing . Clr.suit

    unless (cardSuit == clearingSuit) $ liftErr SuitDoesNotMatch

    -- Take the card from the Marquis' hand
    zoomT (Game.factionCommon Marquis) $ Com.removeCard cardIx

    -- Place a wood token in the clearing
    woodToken <- zoom Game.catFaction Cat.takeWoodToken
    forM_ woodToken $ \w -> do
        zoom (Game.clearingAt clearing) $ Clr.addToken w

----------------------------------
-- Finish Daylight Actions
----------------------------------
execDaylightAction CatFinishDaylightActions = do
    -- Advance the phase
    Game.setPhase $ FactionTurnPhase $ MarquisPhase CatDrawPhase
execDaylightAction _ = liftErr WrongPhase
