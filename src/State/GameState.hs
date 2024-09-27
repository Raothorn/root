module State.GameState (
    -- Initialize
    initialize,
    -- Zooming
    zoomClearings,
    zoomClearing,
    zoomClearing',
    zoomCat,
    zoomBird,
    -- Phase
    getPhase,
    setPhase,
    pushPhase,
    popPhase,
    -- Misc getters
    lookupCard,
    getClearing,
    getClearings,
    getWarriorSupply,
) where

import Control.Monad

import Lens.Micro
import Lens.Micro.Mtl

import Root.Types
import qualified Types.Board as Board
import qualified Types.Faction.FactionCommon as Com
import qualified Types.Faction.Marquis as Cat
import Types.Game
import qualified Types.IxTable as I
import Util

----------------------------------
-- Initialize
----------------------------------
initialize :: [Faction] -> Update Game ()
initialize factions = do
    -- Ensure the phase stack is empty. This should only ever happen with a brand-new Game.
    stack <- use phaseStack
    unless (null stack) $ liftErr WrongPhase

    pushPhase $ SetupPhase factions
    updatePhase

----------------------------------
-- Zooming
----------------------------------
zoomClearings :: (Monoid a) => Update Clearing a -> Update Game a
zoomClearings = zoom (board . Board.clearings . I.traverseTable)

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
getPhase = do
    stack <- use phaseStack
    liftMaybe EmptyPhaseStack (headM stack)

setPhase :: Phase -> Update Game ()
setPhase phase = popPhase' >> pushPhase phase

pushPhase :: Phase -> Update Game ()
pushPhase phase = do
    phaseStack %= (phase :)
    logEvent $ PhasePushed phase

-- updatePhase should be automatically executed after any pop from outside this module
popPhase :: Update Game ()
popPhase = popPhase' >> updatePhase

popPhase' :: Update Game ()
popPhase' = do
    phase <- getPhase
    phaseStack %= tail
    logEvent $ PhasePopped phase

{-
Certain phases are simply tracking state and aren't advanced by actions.
These should be automatically updated behind the scenes.
-}
updatePhase :: Update Game ()
updatePhase = do
    phase <- getPhase
    case phase of
        SetupPhase factions -> updateSetupPhase factions
        TurnPhase turn -> updateTurnPhase turn
        _ -> return ()

{-
Updates: The next faction to be setup is popped off the list. The phase for that
particular faction setup is pushed onto the stack. If there are no more remaining
factions to setup, move onto TurnPhase.
Example: if the phase stack before execution is [SetupPhase [Marquis, Eeerie]], the
phase stack after execution will be [FactionSetupPhase CatSetupPhase, SetupPhase [Eerie]]
-}
updateSetupPhase :: [Faction] -> Update Game ()
updateSetupPhase [] = do
    setPhase $ TurnPhase 0
    updateTurnPhase 0
updateSetupPhase (next : remaining) = do
    setupPhase <- case next of
        Marquis -> return $ FactionSetupPhase CatSetupPhase
        _ -> liftErr NotImplemented
    -- Update the base phase
    setPhase $ SetupPhase remaining
    -- Push the setup phase onto the stack
    pushPhase setupPhase

updateTurnPhase :: Int -> Update Game ()
updateTurnPhase turn = do
    factions <- use factionsInPlay
    let turnFaction = indexMod turn factions

    turnPhase <- case turnFaction of
        Marquis -> return $ FactionTurnPhase $ MarquisPhase CatPlaceWoodPhase
        _ -> liftErr NotImplemented

    -- Update the base phase for the next turn
    setPhase $ TurnPhase (turn + 1)
    -- Push the turn phase onto the stack
    pushPhase turnPhase

----------------------------------
-- Misc Getters
----------------------------------
-- Lookup
lookupCard :: Index Card -> Update Game Card
lookupCard cardIx = do
    f <- use cardLookup
    return $ f cardIx

getClearing :: Index Clearing -> Update Game Clearing
getClearing clearingIx = useMaybe IndexError (board . Board.clearings . I.atTable clearingIx)

getClearings :: Update Game [Clearing]
getClearings = use $ board . Board.clearings . to I.values

getWarriorSupply :: Faction -> Update Game Int
getWarriorSupply faction = do
    common <- getFactionCommon faction
    return $ common ^. Com.warriors

getFactionCommon :: Faction -> Update Game FactionCommon
getFactionCommon Marquis = do
    common <- preuse $ playerFactions . marquis . traversed . Cat.common
    liftMaybe FactionNotInPlay common
getFactionCommon _ = liftErr NotImplemented
