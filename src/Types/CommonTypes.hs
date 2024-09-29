module Types.CommonTypes (
    Suit (..),
    Token (..),
    Building (..),
    Warrior (..),
) where

import Types.Default

-- For common ADTs that don't really deserve their own modules.
-- Unlike the Types.Alias module, these are game-specific.
----------------------------------
-- Types
----------------------------------
data Suit = Mouse | Fox | Rabbit | Bird
    deriving (Eq, Ord, Show)

data Token
    = Wood
    | Keep
    deriving (Eq, Show)

data Warrior
    = CatWarrior
    | BirdWarrior
    deriving (Eq, Show)

data Building
    = Sawmill
    | Workshop
    | Recruiter
    deriving (Eq, Show)

----------------------------------
-- Instances
----------------------------------
instance Default Suit where
    def = Mouse

instance Default Token where
    def = Wood

instance Default Building where
    def = Sawmill
