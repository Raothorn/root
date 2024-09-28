module State.ClearingState (
    addToken,
    addWarrior,
    addBuilding,
    getOppositeCorner,
) where

import Lens.Micro
import Lens.Micro.Mtl

import Root.Types
import Types.Clearing
import Util

----------------------------------
-- Stateful functions
----------------------------------
addToken :: Token -> Update Clearing ()
addToken token = tokens %= (token :)

addWarrior :: Warrior -> Update Clearing ()
addWarrior warrior = warriors %= (warrior :)

addBuilding :: Building -> Update Clearing ()
addBuilding building = do
    numSlots <- use buildingSlots
    numBuildings <- use $ buildings . to length

    if numBuildings >= numSlots
        then liftErr NoFreeBuildingSlots
        else buildings %= (building :)

getOppositeCorner :: Update Clearing (Index Clearing)
getOppositeCorner = useMaybe NotCornerClearing oppositeCorner
