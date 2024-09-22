module Types.Common (
    Suit (..),
    Token(..),
    Building(..)
) where

-- For common ADTs that don't really deserve their own modules.
-- Unlike the Types.Alias module, these are game-specific.

data Suit = Mouse | Fox | Rabbit | Bird
    deriving (Eq, Ord)

data Token = Wood
    deriving (Eq)

data Building = Sawmill | Workshop
    deriving (Eq)
