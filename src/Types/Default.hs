module Types.Default (
    Default (..),
) where

class Default a where
    def :: a
