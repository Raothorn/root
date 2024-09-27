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
    workshopSuits <-
        zoom Game.board $ do
            clearings <- Board.getClearingsP (Clr.hasBuilding Workshop)
            return $ map (^. Clr.suit) clearings

    -- Subtract the already used workshops
    let craftingSuits = bagDifference workshopSuits workshopsUsed

    -- Get the cost of the card
    card <- Game.lookupCard cardIx
    let cardCost = card ^. Card.craftCost

    -- Ensure the Marquis has enough remaining workshops to craft the card
    if bagSubsetOf cardCost craftingSuits
        then do
            Game.zoomCat $ zoom Cat.common $ do
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
