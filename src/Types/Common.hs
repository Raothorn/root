module Types.Common (
    Suit (..),
    Card,
    Token(..),
    Building(..)
) where

-- For common ADTs that don't really deserve their own modules.
-- Unlike the Types.Alias module, these are game-specific.

data Suit = Mouse | Fox | Rabbit | Bird
    deriving (Show, Eq)

type Card = Suit

data Token = Wood
    deriving (Show, Eq)

data Building = Sawmill | Workshop
    deriving (Show, Eq)
