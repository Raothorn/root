{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.ActionTest (
    runActionTests,
) where

import Debug.Trace (traceShowM)

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Data.Functor

import Lens.Micro
import Lens.Micro.Mtl

import Test.Tasty
import Test.Tasty.HUnit

import ExecAction
import Parameters as P
import qualified Root.Card as Card
import qualified Root.Clearing as Clr
import qualified Root.FactionCommon as Com
import qualified Root.Game as Game
import Root.Types
import Test.TestSetup
import Types.IxTable
import Types.LogEvent

debug :: Bool
debug = False

----------------------------------
-- Types
----------------------------------
type M = MaybeT IO

type ActionTest = Game -> M ()

----------------------------------
-- Framework
----------------------------------
runActionTests :: TestTree
runActionTests =
    testGroup
        "action tests"
        ----------------------------------
        -- Cat Setup Happy Path
        ----------------------------------
        [ runTest "Cat setup works with valid input" $ \game -> do
            -- place the keep in the top left corner (1) and the other buildings in the
            -- adjacent clearings (sawmill-2, workshop-4, recruit-7)
            let clearingIxs = (1, 2, 4, 7)
            game <- expect' game $ setupCat clearingIxs
            -- Get all the clearings
            [c1, c2, c4, c7] <- eval game $ do
                forM (clearingIxs ^.. each) $ \cIx -> Game.getClearing (makeIx cIx)

            -- Verify that the keep is in clearing 1
            assertThat $ tokenInClearing Keep c1

            -- Verify that the initial buildings are in the correct clearings
            let clearingBuildings = [(Sawmill, c2), (Workshop, c4), (Recruiter, c7)]
            forM_ clearingBuildings $ \(b, c) -> assertThat $ buildingInClearing b c

            -- Verify that all the warriors have been placed
            allClearingIxs <- eval game $ use Game.allClearingIxs
            let oppositeClearingIx = makeIx 10 :: Index Clearing

            forM_ allClearingIxs $ \clearingIx -> do
                clearing <- eval game $ Game.getClearing clearingIx
                let assertion = warriorInClearing CatWarrior clearing
                if clearingIx == oppositeClearingIx
                    then -- There should be no warrior in the opposite clearing
                        assertNot assertion
                    else -- There should be a warrior in every other clearing
                        assertThat assertion
                return ()

            -- Verify that the number of warriors left in the supply is correct
            -- One warrior should have been placed in all clearings except the opposite one
            let expectedWarriors = P.catWarriorSupply - (length allClearingIxs - 1)
            marquisCommon <- eval game $ Game.getFactionCommon Marquis
            let actualWarriors = marquisCommon ^. Com.warriors
            lift $
                assertEqual
                    "Number of warriors left in supply"
                    expectedWarriors
                    actualWarriors
        , ----------------------------------
          -- Cat Setup Failure Cases
          ----------------------------------
          runTest "Cat setup fails when keep clearing is not a corner" $ \game -> do
            -- Try to put the keep in clearing 2 (not a corner)
            let clearingIxs = (2, 2, 4, 7) & each %~ makeIx
                setupAction = SetupAction $ CatSetupAction clearingIxs
            expectErr NotCornerClearing game $ execAction setupAction
        , runTest "Cat setup fails when building clearing not adjacent to keep" $ \game -> do
            -- Try to put the keep in clearing 1 and the sawmill in clearing 3
            let clearingIxs = (1, 3, 4, 7) & each %~ makeIx
                setupAction = SetupAction $ CatSetupAction clearingIxs
            expectErr InvalidBuildingLocation game $ execAction setupAction
        , ----------------------------------
          -- Cat Place Wood Action
          ----------------------------------
          runTest "Cat place wood action" $ \game -> do
            -- Place the keep in the top left corner (1)
            -- and the sawmill in the adjacent clearing (2)
            game <- expect' game $ setupCat (1, 2, 4, 7)

            -- Place wood in the sawmill clearing
            game <- expect' game placeWood

            -- Verify that the wood token is in the sawmill clearing
            c2 <- eval game $ Game.getClearing $ makeIx 2
            assertThat $ tokenInClearing Wood c2
        , ----------------------------------
          -- Cat Craft Action
          ----------------------------------
          runTest "Cat craft action happy path" $ \game -> do
            -- Place the keep in the top left corner (1)
            -- and the workshop in the adjacent clearing (4)
            game <- expect' game $ setupCat (1, 2, 4, 7) >> placeWood

            -- Get the suit of the workshop clearing
            workshopSuit <- eval game $ do
                c4 <- Game.getClearing $ makeIx 4
                return $ c4 ^. Clr.suit

            -- Replace the card lookup with a card that costs the workshop suit
            -- and has the effect of gaining 3 victory points. We place 2 copies
            -- so that we can test that only one can be crafted.
            let card = Card.newCard def [workshopSuit] (VictoryPoints 3)
            (game, [cardIx, cardIx2]) <- expect game $ replaceCardLookup [card, card]

            -- Give the cards to the Marquis
            game <- expect' game $ do
                Game.giveCard Marquis cardIx
                Game.giveCard Marquis cardIx2

            -- Craft the card
            let craftAction = TurnAction $ MarquisAction $ CatCraft cardIx
            game <- expect' game $ execAction craftAction

            -- Verify that the card is no longer in the Marquis' hand
            expectErr CardNotInHand game $ Game.getHandCard Marquis cardIx

            -- Verify that the Marquis gained 3 victory points
            vps <- eval game $ Game.getVps Marquis
            lift $ assertEqual "Victory points" 3 vps

            -- If the crafting action is tried again with the other card, it should fail
            -- since the Marquis does not have the workshop available to craft it
            let craftAction = TurnAction $ MarquisAction $ CatCraft cardIx2
            expectErr CannotAffordCraft game $ execAction craftAction
        , runTest "Cat craft action fails when card not in hand" $ \game -> do
            game <- expect' game $ setupCatDef >> placeWood

            -- The marquis does not have any cards. Try to craft one anyway.
            let craftAction = TurnAction $ MarquisAction $ CatCraft (makeIx 0)
            expectErr CardNotInHand game $ execAction craftAction
        ]

runTest :: String -> ActionTest -> TestTree
runTest name test = testCase name test'
  where
    -- All of the tests are run with the default forest map
    game = Game.newForestGame
    test' = void $ runMaybeT (initAndRunTest test game)

initAndRunTest :: ActionTest -> ActionTest
initAndRunTest test game = do
    lift $ putStrLn ""
    game <- expect' game $ Game.initialize [Marquis]
    test game

----------------------------------
-- Helpers
----------------------------------
expect :: s -> Update s a -> M (s, a)
expect state f = do
    let result = runWriterT $ runStateT f state
    case result of
        Left err -> do
            lift $ assertString $ "Expected a valid result, but got an error: " <> show err
            nothing
        Right ((x, state'), logs) -> do
            when debug $
                forM_ logs $
                    \log -> lift $ debugEvent log
            return (state', x)

expect' :: s -> Update s a -> M s
expect' state f = expect state f <&> fst

eval :: s -> Update s a -> M a
eval state f = expect state f <&> snd

expectErr :: Error -> s -> Update s a -> M ()
expectErr expectedErr state f = do
    let result = runWriterT $ runStateT f state
    case result of
        Left err ->
            lift $ assertEqual "Got an error, but not the right type" expectedErr err
        Right _ ->
            lift $ assertString "Expected an error, but got a valid result"

expectErr' :: s -> Update s a -> M ()
expectErr' state f = do
    let result = runWriterT $ runStateT f state
    case result of
        Left _ -> return ()
        Right _ ->
            lift $ assertString "Expected an error, but got a valid result"

nothing :: M a
nothing = mzero

----------------------------------
-- Assertions
----------------------------------
type MyAssertion = (Bool, String)

assertThat :: MyAssertion -> M ()
assertThat (p, assert) = lift $ assertBool err p
  where
    err = "Expected: " <> assert

assertNot :: MyAssertion -> M ()
assertNot (p, assert) = lift $ assertBool err (not p)
  where
    err = "Not expected: " <> assert

tokenInClearing :: Token -> Clearing -> MyAssertion
tokenInClearing token clearing = (Clr.hasToken token clearing, assert)
  where
    assert = "Token " <> show token <> " in clearing " <> show (getIx' clearing)

buildingInClearing :: Building -> Clearing -> MyAssertion
buildingInClearing building clearing = (Clr.hasBuilding building clearing, assert)
  where
    assert = "Building " <> show building <> " in clearing " <> show (getIx' clearing)

warriorInClearing :: Warrior -> Clearing -> MyAssertion
warriorInClearing warrior clearing = (Clr.hasWarrior warrior clearing, assert)
  where
    assert = "Warrior " <> show warrior <> " in clearing " <> show (getIx' clearing)

----------------------------------
-- Traces
----------------------------------
tracePhase :: Game -> M ()
tracePhase game = do
    phase <- eval game Game.getPhase
    traceShowM phase
