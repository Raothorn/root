module Ui.View (
    runApp
) where

import Lens.Micro

import Graphics.Vty
import Graphics.Vty.Platform.Unix (mkVty)

import Types

runApp :: IO ()
runApp = do
    vty <- mkVty defaultConfig
    let game = newGame
    let pic = showGame game
    update vty pic

    e <- nextEvent vty
    shutdown vty
    

showGame :: Game -> Picture
showGame game = picForLayers [unitView, bgView]
    where 
        (gameWidth, gameHeight) = (50, 50)
        bgView = charFill defAttr '.' gameWidth gameHeight 
        unitLoc = game ^. unit
        unitView = translate (unitLoc ^. x) (unitLoc ^. y) (char defAttr '@')
