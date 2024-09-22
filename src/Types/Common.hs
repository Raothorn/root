module Types.Common (
    Suit(..),
) where

-- For common ADTs that don't really deserve their own modules.
-- Unlike the Types.Alias module, these are game-specific.

data Suit = Mouse | Fox | Rabbit | Bird
