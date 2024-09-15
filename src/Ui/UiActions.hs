module Ui.UiActions (
    contextActions,
    execUiAction,
) where

import Graphics.Vty

import Lens.Micro
import Lens.Micro.Mtl

import ExecAction
import Types
import qualified Types.Game as G
import Ui.UiTypes

----------------------------------
-- Context Actions
----------------------------------
contextActions :: AppState -> [(Key, UiAction)]
contextActions app =
    case app ^. uiState of
        NoneSelected ->
            [ (KChar 'u', SelectIdleUnit)
            ]
        UnitSelected uid ->
            [ (KLeft, TryAction (MoveUnit uid W))
            , (KRight, TryAction (MoveUnit uid E))
            , (KUp, TryAction (MoveUnit uid N))
            , (KDown, TryAction (MoveUnit uid S))
            , (KEsc, ClearSelect)
            ]

----------------------------------
-- ExecUiAction
----------------------------------
execUiAction :: UiAction -> Update AppState ()
----------------------------------
-- Select Idle Unit
----------------------------------
execUiAction SelectIdleUnit = do
    -- Try to get an idle unit
    idleUnit <- zoom gameState G.getIdleUnit
    -- If successful, select the unit
    mapM_ (\u -> uiState .= UnitSelected u) idleUnit
----------------------------------
-- ClearSelect
----------------------------------
execUiAction ClearSelect = uiState .= NoneSelected
----------------------------------
-- Try Action
----------------------------------
execUiAction (TryAction action) = zoom gameState (execAction action)
