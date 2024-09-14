module Types.Alias (
    Update,
    Con
) where

import Control.Monad.Trans.State.Lazy

import Types.Error

type Update s a = StateT s (Either Error) a

type Con a = Int -> a
