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
execCatAction CatPlaceWoodPhase CatPlaceWood = do
    -- Get all the clearings with a sawmill
    sawmillClearings <- zoom Game.board $
        Board.getClearingsP (Clr.hasBuilding Sawmill)

    forM_ sawmillClearings $ \clearing -> do
        -- Take a wood token, if possible
        woodToken <- Game.zoomCat takeWoodToken

        -- Place the wood token in the clearing
        forM_ woodToken $ \w -> 
            Game.zoomClearing (getIx clearing) (Clr.addToken w)

----------------------------------
-- Otherwise
----------------------------------
execCatAction _ _ = liftErr NotImplemented
