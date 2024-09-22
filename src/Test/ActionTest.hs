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

import Types.Alias
import Types.Error
import Types.Game as G

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
        [ ]

runTest :: String -> ActionTest -> TestTree
runTest name test = testCase name test'
  where
    game = G.newGame
    test' = void $ runMaybeT (test game)

----------------------------------
-- Tests
----------------------------------

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

----------------------------------
-- Queries
----------------------------------

----------------------------------
-- Assertions
----------------------------------
