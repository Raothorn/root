module Types.Alias (
    Update,
    Con,
) where

import Control.Monad.Trans.State.Lazy

import Types.Error

type Update s a = StateT s (Either Error) a

type Con i a = i -> a
