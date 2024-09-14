{-# LANGUAGE RankNTypes #-}

module Util (
    useEither,
    liftErr,
    ifM,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Lens.Micro
import Lens.Micro.Mtl

import Types.Error

useEither :: Error -> SimpleGetter s (Maybe b) -> StateT s (Either Error) b
useEither err l = do
    value <- use l
    case value of
        Just v -> return v
        Nothing -> liftErr err

liftErr :: Error -> StateT s (Either Error) a
liftErr = lift . Left

ifM :: (Monad m) => m Bool -> m () -> m ()
ifM predicate f = do
    result <- predicate
    when result f
