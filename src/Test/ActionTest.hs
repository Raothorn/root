{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.ActionTest (
    runActionTests,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.Functor
import Data.Maybe

import Lens.Micro

import Test.Tasty
import Test.Tasty.HUnit

import ExecAction
import Types
import qualified Types.City as C
import qualified Types.Game as G
import qualified Types.Location as L
import Types.Production
import qualified Types.Unit as U

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
        [ runTest "Move Unit" testMoveUnit
        , runTest "Build City" testBuildCity
        , runTest "Queue Production" testQueueProduction
        , runTest "Advance Turn" testAdvanceTurn
        ]

runTest :: String -> ActionTest -> TestTree
runTest name test = testCase name test'
  where
    game = G.newGame
    test' = void $ runMaybeT (test game)

----------------------------------
-- Tests
----------------------------------
-- We heavily abuse name shadowing here to avoid gross code
testMoveUnit :: ActionTest
testMoveUnit game = do
    -- Add a unit at (0, 0)
    (game, unit) <- addUnitAtOrigin Settler game
    let uid = unit ^. U.unitId

    -- Move the unit south
    let action = MoveUnit uid S
    game <- expect' game $ execAction action
    unit <- eval game $ G.getUnit uid

    -- Check that the unit is now at (0, 1)
    lift $ assertUnitAtLocation (0, 1) unit

    -- Try to move the unit west to (-1, 1). This should trigger an error
    let action = MoveUnit uid W
    expectErr MoveOutOfBoundsError game $ execAction action

testBuildCity :: ActionTest
testBuildCity game = do
    -- Add a settler at (0, 0)
    (game, unit) <- addUnitAtOrigin Settler game
    let uid = unit ^. U.unitId

    -- Build a city at the unit's location
    let action = UnitAction uid BuildCity
    game <- expect' game $ execAction action

    -- Verify that there is a city at the origin
    lift $ assertCityExistsAtLocation (0, 0) game

    -- Verify that the unit no longer exists
    expectErr UnitLookupError game (G.getUnit uid)

testQueueProduction :: ActionTest
testQueueProduction game = do
    -- Add a city
    (game, city) <- addCityAtOrigin game
    let cid = city ^. C.cityId

    -- Verify the city production queue is empty
    lift $
        assertBool
            "The city has a non-empty production queue"
            (isNothing $ city ^. C.productionQueue)

    -- Queue a production in the city
    let action = QueueProduction cid UnitProduction
    game <- expect' game $ execAction action
    city <- eval game $ G.getCity cid

    -- Verify the city has a production
    lift $
        assertBool
            "The city has an empty production queue"
            (isJust $ city ^. C.productionQueue)

testAdvanceTurn :: ActionTest
testAdvanceTurn game = do
    -- Add a city
    (game, city) <- addCityAtOrigin game
    let cid = city ^. C.cityId

    -- Queue a unit production in the city
    game <- expect' game $ execAction (QueueProduction cid UnitProduction)
    city <- eval game $ G.getCity cid

    -- capture how many turns are remaining in the production
    turns <- eval city C.getTurnsRemaining >>= hoistMaybe

    -- Advance the turn
    game <- expect' game G.advanceTurn
    city <- eval game $ G.getCity cid

    -- capture how many turns are remaining after advancing the turn
    turns' <- eval city C.getTurnsRemaining >>= hoistMaybe

    lift $
        assertBool
            "The turn counter has not decremented"
            (turns' < turns)

    -- Verify that there are no units at the same location as the city
    lift $ assertNoUnitAtLocation (0, 0) game

    -- Advance the turn until the production is done
    let advance game _ = expect' game G.advanceTurn
    game <- foldM advance game [1 .. turns']
    city <- eval game $ G.getCity cid

    -- Verify the production queue is empty
    lift $
        assertBool
            "The city still has a production in the queue"
            (isNothing $ city ^. C.productionQueue)

    -- Verify that there was a unit produced at the city
    lift $ assertUnitExistsAtLocation (0, 0) game

----------------------------------
-- Helpers
----------------------------------
expect :: s -> Update s a -> M (s, a)
expect state f = do
    let result = runStateT f state
    case result of
        Left err -> do
            lift $ assertString $ "Expected a valid result, but got an error: " <> show err
            nothing
        Right (x, state') -> do
            return (state', x)

expect' :: s -> Update s a -> M s
expect' state f = expect state f <&> fst

eval :: s -> Update s a -> M a
eval state f = expect state f <&> snd

expectErr :: Error -> s -> Update s a -> M ()
expectErr expectedErr state f = do
    let result = runStateT f state
    case result of
        Left err ->
            lift $ assertEqual "Got an error, but not the right type" expectedErr err
        Right _ ->
            lift $ assertString "Expected an error, but got a valid result"

nothing :: M a
nothing = mzero

----------------------------------
-- Setup
----------------------------------
addUnitAtOrigin :: UnitClass -> Game -> M (Game, Unit)
addUnitAtOrigin uclass game = do
    let conUnit = U.newUnit uclass L.origin
    expect game (G.addIxEntry G.units conUnit)

addCityAtOrigin :: Game -> M (Game, City)
addCityAtOrigin game = do
    let conCity = C.newCity L.origin
    expect game (G.addIxEntry G.cities conCity)

----------------------------------
-- Queries
----------------------------------
unitAtLocation :: (Int, Int) -> Game -> Bool
unitAtLocation loc game = any (\u -> u ^. U.location == Location loc) (game ^. G.units)

----------------------------------
-- Assertions
----------------------------------
assertUnitAtLocation :: (Int, Int) -> Unit -> Assertion
assertUnitAtLocation loc unit = assertEqual errStr (Location loc) (unit ^. U.location)
  where
    errStr = "The unit is not at the expected location"

assertNoUnitAtLocation :: (Int, Int) -> Game -> Assertion
assertNoUnitAtLocation loc game = assertBool errStr (not $ unitAtLocation loc game)
  where
    errStr = "There is a unit at the location, but none was expected"

assertUnitExistsAtLocation :: (Int, Int) -> Game -> Assertion
assertUnitExistsAtLocation loc game = assertBool errStr (unitAtLocation loc game)
  where
    errStr = "There is no unit at the expected location"

assertCityExistsAtLocation :: (Int, Int) -> Game -> Assertion
assertCityExistsAtLocation loc game = assertBool errorStr exists
  where
    exists = any (\city -> city ^. C.location == Location loc) (game ^. G.cities)
    errorStr = "There is no city at the given location"
