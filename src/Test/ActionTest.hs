{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.ActionTest (
    runActionTests
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
import qualified Types.Game as G
import qualified Types.Unit as U
import qualified Types.Location as L

----------------------------------
-- Types
----------------------------------
type M = MaybeT IO 

type ActionTest = Game -> M ()
----------------------------------
-- Framework
----------------------------------
runActionTests :: TestTree
runActionTests = runTest "Test Move Unit" testMoveUnit

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
    --Add a unit at (0, 0)
    let conUnit = U.newUnit Settler L.origin
    (game, unit) <- expect game (G.addUnit conUnit) 
            
    -- Move the unit south
    let action = MoveUnit (unit ^. U.unitId) E
    game <- expect' game $ execAction action
    unit <- eval game $ G.getUnit (unit ^. U.unitId)

    -- Check that the unit is now at (0, 1)
    lift $ assertUnitAtLocation (0, 0) unit


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

nothing :: M a
nothing = mzero

assertUnitAtLocation :: (Int, Int) -> Unit -> Assertion
assertUnitAtLocation loc unit = assertEqual errStr (unit ^. U.location) (Location loc)
    where errStr = "The unit is not at the expected location"


