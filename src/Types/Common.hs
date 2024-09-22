module Types.Common (
    Suit (..),
    Card,
) where

-- For common ADTs that don't really deserve their own modules.
-- Unlike the Types.Alias module, these are game-specific.

data Suit = Mouse | Fox | Rabbit | Bird
    deriving (Show)

type Card = Suit
