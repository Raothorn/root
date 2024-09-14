module Ui.View (
    showGame
) where

import Lens.Micro

import Graphics.Vty

import Types

showGame :: Game -> Picture
showGame game = picForLayers [unitView, bgView]
    where 
        (gameWidth, gameHeight) = (50, 25) :: (Int, Int)
        bgView = charFill defAttr '.' gameWidth gameHeight 
        unitLoc = game ^. unit
        unitView = translate (unitLoc ^. x) (unitLoc ^. y) (char defAttr '@')
