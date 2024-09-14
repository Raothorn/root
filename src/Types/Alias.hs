module Types.Alias (
    Update
) where

import Control.Monad.Trans.State.Lazy

import Types.Error

type Update s a = StateT s (Either Error) a
