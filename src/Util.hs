{-# LANGUAGE RankNTypes #-}

module Util (
    -- Failure
    useEither,
    liftErr,
    liftMaybe,
    -- Control
    ifM,
    -- IxTable
    addIxEntry,
    getIxEntry,
    deleteIxEntry,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Lens.Micro
import Lens.Micro.Mtl

-- Only import game-agnostic types
import Types.Alias
import Types.Error
import Types.IxTable as I

----------------------------------
-- Failure Utilities
----------------------------------
useEither :: Error -> SimpleGetter s (Maybe b) -> Update s b
useEither err l = do
    value <- use l
    case value of
        Just v -> return v
        Nothing -> liftErr err

liftErr :: Error -> Update s b
liftErr = lift . Left

liftMaybe :: Maybe a -> Update s a
liftMaybe x = do
    let eitherX = maybeToEither Error x
    lift eitherX

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither err = maybe (Left err) Right

----------------------------------
-- Control Utilities
----------------------------------
ifM :: (Monad m) => m Bool -> m () -> m ()
ifM predicate f = do
    result <- predicate
    when result f

----------------------------------
-- IxTable Utilities
----------------------------------
-- Lensy CRUD operations on IxTable
addIxEntry :: (Id i) => Lens' s (IxTable a) -> Con i a -> Update s a
addIxEntry l con = do
    table <- use l
    let (table', x) = I.insert con table
    l .= table'
    return x

getIxEntry :: (Id i) => Error -> Lens' s (IxTable a) -> i -> Update s a
getIxEntry err l entryId = useEither err $ l . atTable entryId

deleteIxEntry :: (Id i) => Lens' s (IxTable a) -> i -> Update s ()
deleteIxEntry l entryId = l %= I.delete entryId
