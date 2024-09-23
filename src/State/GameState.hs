module State.GameState (
    -- Zooming
    zoomClearing,
    zoomClearing',
    zoomCat,
    zoomBird,
    -- Phase
    getPhase,
    setPhase,
    pushPhase,
    popPhase,
) where

import Data.Maybe

import Lens.Micro
import Lens.Micro.Mtl

import Root.Types
import qualified Types.Board as Board
import Types.Game
import qualified Types.IxTable as I

----------------------------------
-- Zooming
----------------------------------
zoomClearing :: (Monoid a) => Index Clearing -> Update Clearing a -> Update Game a
zoomClearing i = zoom (board . Board.clearings . I.ixTable i)

zoomClearing' :: (Monoid a) => Clearing -> Update Clearing a -> Update Game a
zoomClearing' = zoomClearing . getIx

zoomCat :: (Monoid a) => Update CatFaction a -> Update Game a
zoomCat = zoom (playerFactions . marquis . traversed)

zoomBird :: (Monoid a) => Update BirdFaction a -> Update Game a
zoomBird = zoom (playerFactions . eerie . traversed)

----------------------------------
-- Phase
----------------------------------
-- Under normal operations, the phase stack should never become empty.
-- It is on us as the developer to make sure that pushes and pops
-- happen in pairs. The easiest way to do this is to use "setPhase"
-- whenever possible, instead of pushPhase and popPhase
getPhase :: Update Game Phase
getPhase = preuse (phaseStack . _head) <&> fromMaybe NoPhase

setPhase :: Phase -> Update Game ()
setPhase phase = popPhase >> pushPhase phase

pushPhase :: Phase -> Update Game ()
pushPhase phase = phaseStack %= (phase :)

popPhase :: Update Game ()
popPhase = phaseStack %= tail
