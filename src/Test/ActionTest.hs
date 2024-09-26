{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.ActionTest (
    runActionTests,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Data.Functor

import Lens.Micro

import Test.Tasty
import Test.Tasty.HUnit

import ExecAction
import qualified Root.Clearing as Clr
import qualified Root.Game as Game
import Root.Types
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
        [ runTest "Cat Setup" testCatSetup
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
-- Tests
----------------------------------
testCatSetup :: ActionTest
testCatSetup game = do
    -- Place the keep in the top left corner (1) and the other buildings in the
    -- adjacent clearings (sawmill-2, workshop-4, recruit-7)
    let clearingIxs = (1, 2, 4, 7) & each %~ makeIx
        setupAction = SetupAction $ CatSetupAction clearingIxs
    game <- expect' game $ execAction setupAction

    -- Get all the clearings
    [c1, c2, c4, c7] <- eval game $ do
        forM (clearingIxs ^.. each) $ \cIx -> Game.getClearing cIx

    -- Verify that the keep is in clearing 1
    assertThat $ tokenInClearing Keep c1

    -- Verify that the initial buildings are in the correct clearings
    let clearingBuildings = [(Sawmill, c2), (Workshop, c4), (Recruiter, c7)]
    forM_ clearingBuildings $ \(b, c) -> assertThat $ buildingInClearing b c

    -- Verify that all the warriors have been placed
    allClearings <- eval game Game.getClearings
    let oppositeClearingIx = makeIx 10 :: Index Clearing

    forM_ allClearings $ \clearing -> do
        let assertion = warriorInClearing CatWarrior clearing
        if getIx clearing == oppositeClearingIx
            then -- There should be no warrior in the opposite clearing
                assertNot assertion
            else -- There should be a warrior in every other clearing
                assertThat assertion

testCatSetupFails :: ActionTest
testCatSetupFails game = do
    -- Test that the setup fails when the keep clearing is not a corner
    nothing

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
-- Helpers
----------------------------------
