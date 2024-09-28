{-# LANGUAGE RankNTypes #-}

module Util (
    -- Monad Stack
    useMaybe,
    liftErr,
    liftMaybe,
    liftTraversal,
    logEvent,
    zoomT,
    -- Control
    whenM,
    -- List
    bagDifference,
    bagSubsetOf,
    headM,
    indexMod,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Data.Maybe
import qualified Data.MultiSet as MS

import Lens.Micro
import Lens.Micro.Mtl

import Types.Alias
import Types.Error
import Types.LogEvent

----------------------------------
-- Monad Stack Utilities
----------------------------------
liftErr :: Error -> Update s a
liftErr = lift . lift . Left

liftMaybe :: Error -> Maybe a -> Update s a
liftMaybe err mValue =
    case mValue of
        Just v -> return v
        Nothing -> liftErr err

useMaybe :: Error -> SimpleGetter s (Maybe a) -> Update s a
useMaybe err l = do
    x <- use l
    liftMaybe err x

liftTraversal :: Error -> Traversal' s a -> Update s a
liftTraversal err l = do
    x <- preuse l
    liftMaybe err x

logEvent :: LogEvent -> Update s ()
logEvent event = lift $ writer ((), [event])

zoomT :: Traversal' s a -> Update a b -> Update s b
zoomT l f = do
    result <- zoom l $ pure <$> f
    liftMaybe TraversalError (listToMaybe result)

----------------------------------
-- Control Utilities
----------------------------------
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM predicate f = do
    result <- predicate
    when result f

----------------------------------
-- List Utilities
----------------------------------
bagDifference :: (Ord a) => [a] -> [a] -> [a]
bagDifference items removeItems = MS.toList (MS.difference items' removeItems')
  where
    items' = MS.fromList items
    removeItems' = MS.fromList removeItems

bagSubsetOf :: (Ord a) => [a] -> [a] -> Bool
bagSubsetOf l1 l2 = MS.isSubsetOf l1' l2'
  where
    l1' = MS.fromList l1
    l2' = MS.fromList l2

headM :: [a] -> Maybe a
headM = listToMaybe

-- Technically unsafe but will never error
indexMod :: Int -> [a] -> a
indexMod n xs = xs !! n'
  where
    n' = n `mod` length xs
