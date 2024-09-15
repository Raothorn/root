module Ui.View.AppView (
    showApp,
) where

import Graphics.Vty

import Lens.Micro

import Ui.UiActions
import Ui.UiTypes
import Ui.View.GameView
import Ui.VtyUtil

showApp :: AppState -> Image
showApp app = gameImg <-> actionImg
  where
    gameImg = showGame (app ^. uiState) (app ^. gameState)
    actionImg = showContextActions app

showContextActions :: AppState -> Image
showContextActions app = vertCat $ map showAction actions
  where
    actions = contextActions app
    showAction (key, action) = defStr $ showKey key <> ": " <> show action

showKey :: Key -> String
showKey (KChar c) = pure c
showKey KLeft = "Left"
showKey KRight = "Right"
showKey KUp = "Up"
showKey KDown = "Down"
showKey KEsc = "Esc"
showKey k = show k
