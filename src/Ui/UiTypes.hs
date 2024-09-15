{-# LANGUAGE TemplateHaskell #-}

module Ui.UiTypes (
    UiState (..),
    UiAction (..),
    AppState,
    -- Lenses
    uiState,
    gameState,
    -- Constructors
    newApp,
    -- Helpers
    isUnitSelected
) where

import Lens.Micro
import Lens.Micro.TH

import Types
import qualified Types.Game as G
import qualified Types.Unit as U
import qualified Types.Location as L

----------------------------------
-- Types
----------------------------------
data UiState
    = UnitSelected UnitId
    | NoneSelected

data AppState = AppState
    { _uiState :: UiState
    , _gameState :: Game
    }

data UiAction
    = SelectIdleUnit
    | ClearSelect
    | TryAction Action
    deriving (Show)

----------------------------------
-- Lenses
----------------------------------
makeLenses ''AppState

----------------------------------
-- Constructors
----------------------------------
newApp :: AppState
newApp = AppState NoneSelected initGame
    where 
        unitCon = U.newUnit Settler L.origin
        initGame = G.newGame & G.units %~ insert' unitCon

----------------------------------
-- Helpers
----------------------------------
isUnitSelected :: Unit -> UiState -> Bool
isUnitSelected u (UnitSelected b) = u ^. U.unitId == b
isUnitSelected _ _ = False
