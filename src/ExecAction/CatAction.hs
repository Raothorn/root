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
import qualified Root.Phase as Phase
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
execDaylightAction CatBuild = liftErr NotImplemented
execDaylightAction CatOverwork = liftErr NotImplemented
execDaylightAction CatFinishDaylightActions = do
    -- Advance the phase
    Game.setPhase $ FactionTurnPhase $ MarquisPhase CatDrawPhase
execDaylightAction _ = liftErr WrongPhase
