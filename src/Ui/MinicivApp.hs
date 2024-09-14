module Ui.MinicivApp (
    runApp,
) where

import Control.Monad
import Control.Monad.Trans.State.Lazy

import Graphics.Vty
import Graphics.Vty.Platform.Unix (mkVty)

import ExecAction
import Types
import Ui.Event
import Ui.View

runApp :: IO ()
runApp = do
    vty <- mkVty defaultConfig
    let game = newGame
    updateApp vty game
    shutdown vty

updateApp :: Vty -> Game -> IO ()
updateApp vty game = do
    let pic = showGame game
    update vty pic

    e <- nextEvent vty
    let action = eventAction e

    unless (isQuit action) $ do
        let game' = execStateT (execAction action) game

        case game' of
            Left _   -> return ()
            Right g' -> updateApp vty g'
