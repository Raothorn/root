module Ui.MinicivApp (
    runApp,
) where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import qualified Data.List as List

import Graphics.Vty
import Graphics.Vty.Platform.Unix (mkVty)

import Ui.UiActions
import Ui.UiTypes
import Ui.View.AppView

----------------------------------
-- IO
----------------------------------

runApp :: IO ()
runApp = do
    vty <- mkVty defaultConfig
    let app = newApp
    updateApp vty app
    shutdown vty

updateApp :: Vty -> AppState -> IO ()
updateApp vty app = do
    let pic = picForImage $ showApp app
    update vty pic

    e <- nextEvent vty
    app' <- handleEvent e app
    forM_ app' $ updateApp vty

handleEvent :: Event -> AppState -> IO (Maybe AppState)
handleEvent (EvKey (KChar 'q') _) _ = return Nothing
handleEvent (EvKey k _) app = do
    let actions = contextActions app
        action = List.lookup k actions

    case action of 
        Just a -> do 
            let actionResult = execStateT (execUiAction a) app
            case actionResult of
                -- TODO handle error gracefully
                Left _ -> return $ Just app
                Right app' -> return $ Just app'
        Nothing -> return $ Just app

handleEvent _ app = return $ Just app
