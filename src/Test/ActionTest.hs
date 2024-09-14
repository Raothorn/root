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

import Lens.Micro

import Test.Tasty
import Test.Tasty.HUnit

import ExecAction
import Types
import qualified Types.City as C
import qualified Types.Game as G
import qualified Types.Location as L
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

----------------------------------
-- Helpers
----------------------------------
expect :: Game -> Update Game a -> M (Game, a)
expect game f = do
    let result = runStateT f game
    case result of
        Left err -> do
            lift $ assertString $ "Expected a valid result, but got an error: " <> show err
            nothing
        Right (x, game') -> do
            return (game', x)

expect' :: Game -> Update Game a -> M Game
expect' game f = expect game f <&> fst

eval :: Game -> Update Game a -> M a
eval game f = expect game f <&> snd

expectErr :: Error -> Game -> Update Game a -> M ()
expectErr expectedErr game f = do
    let result = runStateT f game
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

----------------------------------
-- Assertions
----------------------------------
assertUnitAtLocation :: (Int, Int) -> Unit -> Assertion
assertUnitAtLocation loc unit = assertEqual errStr (Location loc) (unit ^. U.location)
  where
    errStr = "The unit is not at the expected location"

assertCityExistsAtLocation :: (Int, Int) -> Game -> Assertion
assertCityExistsAtLocation loc game = assertBool errorStr exists
  where
    exists = any (\city -> city ^. C.location == Location loc) (game ^. G.cities)
    errorStr = "There is no city at the given location"
