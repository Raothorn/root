module Types.LogEvent (
    LogEvent (..),
) where

import Types.Card (Card)
import Types.Clearing (Clearing)
import Types.Faction (Faction)
import Types.IxTable (Index)

data LogEvent
    = -- General events
      CardCrafted (Index Card) Faction
    | -- Cat events
      WoodPlaced (Index Clearing)
