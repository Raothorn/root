module Ui.MinicivApp (
    runApp
) where
 
import Graphics.Vty
import Graphics.Vty.Platform.Unix (mkVty)

import Ui.View

import Types

runApp :: IO ()
runApp = do
    vty <- mkVty defaultConfig
    let game = newGame
    let pic = showGame game
    update vty pic

    e <- nextEvent vty
    shutdown vty
