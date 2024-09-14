module Ui.Event (
    -- eventAction,
) where

import Graphics.Vty

import Types

-- eventAction :: Event -> Action
-- ----------------------------------
-- -- Movement
-- ----------------------------------
-- eventAction (EvKey KUp _) = MoveUnit 0 N
-- eventAction (EvKey KDown _) = MoveUnit 0 S
-- eventAction (EvKey KLeft _) = MoveUnit 0 W
-- eventAction (EvKey KRight _) = MoveUnit 0 E
--
-- ----------------------------------
-- -- Temp
-- ----------------------------------
-- eventAction (EvKey (KChar ' ') _) = UnitAction 0 BuildCity
--
-- ----------------------------------
-- -- Quit
-- ----------------------------------
-- eventAction (EvKey (KChar 'q') _) = QuitAction
-- eventAction (EvKey KEsc _) = QuitAction
--
-- eventAction _ = NoAction
