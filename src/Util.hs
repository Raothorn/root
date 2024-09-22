{-# LANGUAGE RankNTypes #-}

module Util (
    -- Failure
    useEither,
    liftErr,
    liftMaybe,
    -- Control
    whenM,
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
useEither :: Error -> SimpleGetter s (Maybe a) -> Update s a
useEither err l = do
    value <- use l
    case value of
        Just v -> return v
        Nothing -> liftErr err

liftErr :: Error -> Update s a
liftErr = lift . Left

liftMaybe :: Maybe a -> Update s a
liftMaybe x = do
    let eitherX = maybeToEither Error x
    lift eitherX

maybeToEither :: Error -> Maybe a -> Either Error a
maybeToEither err = maybe (Left err) Right

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
getIxEntry err l entryId = useEither err $ l . atTable entryId

deleteIxEntry :: Lens' s (IxTable a) -> Index a -> Update s ()
deleteIxEntry l entryId = l %= I.delete entryId
