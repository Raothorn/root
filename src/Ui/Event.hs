module Ui.Event (
    eventAction,
) where

import Graphics.Vty

import Types

eventAction :: Event -> Action
----------------------------------
-- Movement
----------------------------------
eventAction (EvKey KUp _) = MoveUnit N
eventAction (EvKey KDown _) = MoveUnit S
eventAction (EvKey KLeft _) = MoveUnit W
eventAction (EvKey KRight _) = MoveUnit E

----------------------------------
-- Quit
----------------------------------
eventAction (EvKey (KChar 'q') _) = QuitAction
eventAction (EvKey KEsc _) = QuitAction

eventAction _ = NoAction
