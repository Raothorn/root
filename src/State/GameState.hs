module State.GameState (
    -- Initialize
    initialize,
    -- Common Actions
    takeWarriorFromSupplyAndPlace,
    takeWarriorAndReturnToSupply,
    giveCard,
    initiateBattle,
    moveWarriors,
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
    areClearingsConnected,
) where

import Control.Monad
import Data.Maybe

import Lens.Micro
import Lens.Micro.Mtl

import qualified Root.Clearing as Clr
import qualified Root.FactionCommon as Com
import Root.Types
import Types.Game
import qualified Types.Phase as Phase
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
takeWarriorFromSupplyAndPlace :: Faction -> Index Clearing -> Update Game ()
takeWarriorFromSupplyAndPlace faction clearingIx = do
    warrior <- zoomT (factionCommon faction) Com.removeWarrior

    forM_ warrior $ \w ->
        zoom (clearingAt clearingIx) $ Clr.addWarrior w

takeWarriorAndReturnToSupply :: Faction -> Index Clearing -> Update Game ()
takeWarriorAndReturnToSupply faction clearingIx = do
    warrior <- zoomT (clearingAt clearingIx) $ Clr.removeWarrior faction

    forM_ warrior $ \w ->
        zoomT (factionCommon faction) $ Com.addWarrior w

giveCard :: Faction -> Index Card -> Update Game ()
giveCard faction cardIx = do
    zoom (factionCommon faction) $ Com.addCard cardIx

initiateBattle :: Faction -> Faction -> Index Clearing -> Update Game ()
initiateBattle attacker defender clearingIx = do
    -- Verify that the attacker has at least one warrior
    -- and that the defender has at least one piece
    (attackerWarriors, defenderPieces) <- zoomT (clearingAt clearingIx) $ do
        attackerWarriors <- Clr.numFactionWarriors attacker
        defenderPieces <- Clr.numFactionPieces defender
        return (attackerWarriors, defenderPieces)

    when (attackerWarriors <= 0) $ liftErr NoWarriorsInClearing
    when (defenderPieces <= 0) $ liftErr NoPiecesInClearing

    -- Push the battle phase onto the stack
    let phase = Phase.newBattlePhase attacker defender clearingIx
    pushPhase phase

{-
Rule 4.2: When you move, you may take any number of your warriors or your pawn
from one clearing and move them to one adjacent clearing.

You Must Rule. To take a move, you must rule the origin clearing, destination clearing, or both.
No Movement Limits. A given piece can be moved any number of times per turn. If you are prompted
to take multiple moves, you may move the same or separate groups of warriors.
-}
moveWarriors :: Faction -> Int -> Index Clearing -> Index Clearing -> Update Game ()
moveWarriors faction numWarriors fromIx toIx = do
    -- Verify that the faction rules either the origin clearing or the destination clearing
    fromRuler <- zoomT (clearingAt fromIx) Clr.getRulingFaction
    toRuler <- zoomT (clearingAt toIx) Clr.getRulingFaction

    unless (elem faction $ catMaybes [fromRuler, toRuler]) $
        liftErr CannotMoveDueToRule

    -- Remove the warriors from the origin clearing
    warriors <-
        zoomT (clearingAt fromIx) $
            Clr.removeWarriors faction numWarriors

    -- Add the warriors to the destination clearing
    forM_ warriors $ \w ->
        zoom (clearingAt toIx) $ Clr.addWarrior w

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
{-
Returns true if the clearings are connected by a path of clearings that the faction rules.
-}
areClearingsConnected ::
    Faction ->
    Index Clearing ->
    Index Clearing ->
    [Index Clearing] ->
    Update Game Bool
areClearingsConnected faction clearingIx targetIx breadcrumbs = do
    ruler <- zoomT (clearingAt clearingIx) Clr.getRulingFaction
    adjacentClearings <- use (clearingAt clearingIx . Clr.adjacent)
    if ruler == Just faction && clearingIx == targetIx
        then return True
        else if ruler /= Just faction || elem clearingIx breadcrumbs
            then return False
            else do
                let breadcrumbs' = clearingIx : breadcrumbs
                connected <- forM adjacentClearings $ \adjIx ->
                    areClearingsConnected faction adjIx targetIx breadcrumbs'
                return $ or connected
