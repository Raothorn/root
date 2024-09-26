module Types.Alias (
    Update,
) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy

import Types.Error
import Types.LogEvent

type Update s a = StateT s (WriterT [LogEvent] (Either Error)) a
