module ExecAction.CatAction (
    execCatAction
) where

import Control.Monad
import Control.Monad.Trans.State.Lazy

import Lens.Micro
import Lens.Micro.Mtl

import Types
import Types.Faction.Marquis
import qualified Types.Game as Game
import qualified Types.RootBoard as Board
import qualified Types.Clearing as Clr
import Util
import Lookup.CardLookup
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
-- just allocate all the tokens arbitrarily.

----------------------------------
-- Place Wood
----------------------------------
execCatAction CatPlaceWoodPhase CatPlaceWood = do
    -- Get all the clearings with a sawmill
    sawmillClearings <- zoom Game.board $
        Board.getClearingsP (Clr.hasBuilding Sawmill)

    forM_ sawmillClearings $ \clearing -> do
        -- Take a wood token, if possible
        woodToken <- Game.zoomCat takeWoodToken

        -- Place the wood token in the clearing
        forM_ woodToken $ \w -> 
            Game.zoomClearing' clearing (Clr.addToken w)

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
execCatAction (CatCraftPhase suitsUsed) (CatCraft cardIx) = do
    let card = lookupCard cardIx

    -- Get all of the clearings with a workshop
    craftingClearings <- zoom Game.board $
        Board.getClearingsP (Clr.hasBuilding Workshop)
    let craftingSuits = map (^. Clr.suit) craftingClearings 
        remainingSuits = bagDifference craftingSuits suitsUsed

    return ()

----------------------------------
-- Otherwise
----------------------------------
execCatAction _ _ = liftErr NotImplemented
