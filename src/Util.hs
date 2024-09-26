{-# LANGUAGE RankNTypes #-}

module Util (
    -- Monad Stack
    useMaybe,
    liftErr,
    liftMaybe,
    logEvent,
    listReturn,
    -- Control
    whenM,
    -- IxTable
    addIxEntry,
    getIxEntry,
    deleteIxEntry,
    -- List
    bagDifference,
    bagSubsetOf,
    headM,
    indexMod,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Lazy
import Data.Maybe
import qualified Data.MultiSet as MS

import Lens.Micro
import Lens.Micro.Mtl

import Types.Alias
import Types.Error
import Types.IxTable as I
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

logEvent :: LogEvent -> Update s ()
logEvent event = lift $ writer ((), [event])

{-
Notes: Since the monoid instance of Maybe doesn't really work, we wrap with a list
as a workaround for any stateful function with a non-empty, non-monoidal return type
so that we can easily use these within a "zoomed" context.
-}
listReturn :: Update s a -> Update s [a]
listReturn f = do
    x <- f
    return [x]

----------------------------------
-- Control Utilities
----------------------------------
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM predicate f = do
    result <- predicate
    when result f

----------------------------------
-- IxTable Utilities
----------------------------------
-- Lensy CRUD operations on IxTable
addIxEntry :: Lens' s (IxTable a) -> ConIx a -> Update s a
addIxEntry l con = do
    table <- use l
    let (table', x) = I.insert con table
    l .= table'
    return x

getIxEntry :: Error -> Lens' s (IxTable a) -> Index a -> Update s a
getIxEntry err l entryId = useMaybe err $ l . atTable entryId

deleteIxEntry :: Lens' s (IxTable a) -> Index a -> Update s ()
deleteIxEntry l entryId = l %= I.delete entryId

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

-- Technicall unsafe but will never error
indexMod :: Int -> [a] -> a
indexMod n xs = xs !! n
  where
    n' = n `mod` length xs
