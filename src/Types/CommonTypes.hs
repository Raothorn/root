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
data Suit = Mouse | Fox | Rabbit | Bird | NoSuit
    deriving (Eq, Ord, Show)

data Token
    = Wood
    | Keep
    | NoToken
    deriving (Eq, Show)

data Warrior
    = CatWarrior
    | BirdWarrior
    | NoWarrior
    deriving (Eq, Show)

data Building
    = Sawmill
    | Workshop
    | Recruiter
    | NoBuilding
    deriving (Eq, Show)

----------------------------------
-- Instances
----------------------------------
instance Default Suit where
    def = NoSuit

instance Default Token where
    def = NoToken

instance Default Building where
    def = NoBuilding
