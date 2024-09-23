module Types.CommonTypes (
    Suit (..),
    Token (..),
    Building (..),
) where

import Types.Default

-- For common ADTs that don't really deserve their own modules.
-- Unlike the Types.Alias module, these are game-specific.
----------------------------------
-- Types
----------------------------------
data Suit = Mouse | Fox | Rabbit | Bird | NoSuit
    deriving (Eq, Ord)

data Token = Wood | NoToken
    deriving (Eq)

data Building = Sawmill | Workshop | NoBuilding
    deriving (Eq)

----------------------------------
-- Instances
----------------------------------
instance Default Suit where
    def = NoSuit

instance Default Token where
    def = NoToken

instance Default Building where
    def = NoBuilding
