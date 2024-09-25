module Lookup.BoardLookup (
    forestMap
) where

import Lens.Micro
import Types.Clearing
import Types.CommonTypes
import Types.IxTable as I

forestMap :: IxTable Clearing
forestMap = fmap (\c -> initClearing (getIx' c) c) clearings
  where
    clearings = I.createN newClearing 1 12

initClearing :: Int -> Clearing -> Clearing
----------------------------------
-- Clearing 1
----------------------------------
initClearing 1 =
    (adjacent .~ [makeIx 2, makeIx 4, makeIx 7])
        . (isCorner .~ True)
        . (suit .~ Fox)
        . (buildingSlots .~ 1)
----------------------------------
-- Clearing 2
----------------------------------
initClearing 2 =
    (adjacent .~ [makeIx 1, makeIx 3, makeIx 5])
        . (isCorner .~ False)
        . (suit .~ Mouse)
        . (buildingSlots .~ 2)
----------------------------------
-- Clearing 3
----------------------------------
initClearing 3 =
    (adjacent .~ [makeIx 2, makeIx 5, makeIx 6])
        . (isCorner .~ True)
        . (suit .~ Rabbit)
        . (buildingSlots .~ 1)
----------------------------------
-- Clearing 4
----------------------------------
initClearing 4 =
    (adjacent .~ [makeIx 1, makeIx 5, makeIx 12])
        . (isCorner .~ False)
        . (suit .~ Rabbit)
        . (buildingSlots .~ 2)
----------------------------------
-- Clearing 5
----------------------------------
initClearing 5 =
    (adjacent .~ [makeIx 2, makeIx 4, makeIx 8, makeIx 9, makeIx 3])
        . (isCorner .~ False)
        . (suit .~ Fox)
        . (buildingSlots .~ 2)
----------------------------------
-- Clearing 6
----------------------------------
initClearing 6 =
    (adjacent .~ [makeIx 3, makeIx 9])
        . (isCorner .~ False)
        . (suit .~ Fox)
        . (buildingSlots .~ 2)
----------------------------------
-- Clearing 7
----------------------------------
initClearing 7 =
    (adjacent .~ [makeIx 1, makeIx 12])
        . (isCorner .~ False)
        . (suit .~ Rabbit)
        . (buildingSlots .~ 2)
----------------------------------
-- Clearing 8
----------------------------------
initClearing 8 =
    (adjacent .~ [makeIx 5, makeIx 10, makeIx 11])
        . (isCorner .~ False)
        . (suit .~ Mouse)
        . (buildingSlots .~ 3)
----------------------------------
-- Clearing 9
----------------------------------
initClearing 9 =
    (adjacent .~ [makeIx 10, makeIx 5, makeIx 6])
        . (isCorner .~ False)
        . (suit .~ Mouse)
        . (buildingSlots .~ 2)
----------------------------------
-- Clearing 10
----------------------------------
initClearing 10 =
    (adjacent .~ [makeIx 9, makeIx 8, makeIx 11])
        . (isCorner .~ True)
        . (suit .~ Rabbit)
        . (buildingSlots .~ 1)
----------------------------------
-- Clearing 11
----------------------------------
initClearing 11 =
    (adjacent .~ [makeIx 12, makeIx 8, makeIx 10])
        . (isCorner .~ False)
        . (suit .~ Fox)
        . (buildingSlots .~ 2)
----------------------------------
-- Clearing 12
----------------------------------
initClearing 12 =
    (adjacent .~ [makeIx 7, makeIx 4, makeIx 11])
        . (isCorner .~ True)
        . (suit .~ Mouse)
        . (buildingSlots .~ 2)

initClearing _ = id
