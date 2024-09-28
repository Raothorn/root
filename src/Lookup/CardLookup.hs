module Lookup.CardLookup (
    lookupCard,
) where

import Types.Card
import Types.Default
import Types.Index

lookupCard :: Index Card -> Card
lookupCard = const def
