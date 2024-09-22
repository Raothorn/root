module Lookup.CardLookup (
    lookupCard
) where

import Types.IxTable (Index)
import Types.Card

lookupCard :: Index Card -> Card
lookupCard _ = newCard
