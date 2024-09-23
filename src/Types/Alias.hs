module Types.Alias (
    Update,
) where

import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy

import Types.LogEvent
import Types.Error

type Update s a = StateT s (WriterT [LogEvent] (Either Error)) a
