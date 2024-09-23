module ExecAction.CatAction (
    execCatAction,
) where

import Control.Monad

import Lens.Micro
import Lens.Micro.Mtl

import Lookup.CardLookup
import Types
import qualified Types.Card as Card
import qualified Types.Clearing as Clr
import Types.Faction
import Types.Faction.FactionCommon as Com
import qualified Types.Faction.Marquis as Cat
import qualified Types.Game as Game
import qualified Types.RootBoard as Board
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
            Game.logEvent $ WoodPlaced (getIx clearing)

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

            -- Log the crafting Action
            Game.logEvent (CardCrafted cardIx Marquis)
            -- Update "workshopsUsed"
            Game.setPhase $ MarquisPhase $ CatCraftPhase (workshopsUsed ++ cardCost)
        else liftErr CannotAffordCraft

----------------------------------
-- Otherwise
----------------------------------
execCatAction _ _ = liftErr NotImplemented
