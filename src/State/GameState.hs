module State.GameState (
    -- Initialize
    initialize,
    -- Common Actions
    placeWarrior,
    giveCard,
    -- Phase
    getPhase,
    setPhase,
    pushPhase,
    popPhase,
    -- Lookup
    lookupCard,
    -- Getters
    getClearing,
    getClearingsWhere,
    getFactionCommon,
    getHandCard,
    getVps,
) where

import Control.Monad

import Lens.Micro
import Lens.Micro.Mtl

import qualified Root.Clearing as Clr
import qualified Root.FactionCommon as Com
import Root.Types
import qualified Types.Board as Board
import Types.Game
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
-- Common Actions
----------------------------------
placeWarrior :: Faction -> Index Clearing -> Update Game ()
placeWarrior faction clearingIx = do
    warrior <- zoomT (factionCommon faction) Com.removeWarrior

    forM_ warrior $ \w ->
        zoom (clearingAt clearingIx) $ Clr.addWarrior w

giveCard :: Faction -> Index Card -> Update Game ()
giveCard faction cardIx = do
    zoom (factionCommon faction) $ Com.addCard cardIx

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
-- Lookup
----------------------------------
lookupCard :: Index Card -> Update Game Card
lookupCard cardIx = do
    f <- use cardLookup
    return $ f cardIx

----------------------------------
-- Misc. Getters
----------------------------------
-- Lifting various failable traversals into the monadic context
getClearing :: Index Clearing -> Update Game Clearing
getClearing i = liftTraversal IndexError $ clearingAt i

getClearingsWhere :: (Clearing -> Bool) -> Update Game [Index Clearing]
getClearingsWhere p = do
    zoom (allClearings . filtered p) $ do
        index <- use Clr.index
        return [index]

getFactionCommon :: Faction -> Update Game FactionCommon
getFactionCommon faction = liftTraversal FactionNotInPlay $ factionCommon faction

getHandCard :: Faction -> Index Card -> Update Game Card
getHandCard faction cardIx = do
    hand <- use $ factionCommon faction . Com.hand
    if cardIx `elem` hand
        then lookupCard cardIx
        else liftErr CardNotInHand

getVps :: Faction -> Update Game Int
getVps faction =
    liftTraversal FactionNotInPlay $
        factionCommon faction . Com.victoryPoints
