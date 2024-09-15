module Ui.View.GameView (
    showGame,
) where

import Control.Monad.Trans.State.Lazy

import Lens.Micro
import Lens.Micro.Mtl

import Graphics.Vty

import Types
import qualified Types.Game as G
import qualified Types.Unit as U
import Ui.UiTypes

----------------------------------
-- Types
----------------------------------
type Tile = (Char, Attr)

type Grid = [[Tile]]

----------------------------------
-- Constructors
----------------------------------

showGame :: UiState -> Game -> Image
showGame ui game = vertCat $ map showLine grid
  where
    grid = execState (showGameGrid ui game) (fillBg 30 15)
    showTile (c, attr) = char attr c
    showLine = horizCat . map showTile

fillBg :: Int -> Int -> Grid
fillBg width height = replicate height (replicate width ('.', defAttr))

----------------------------------
-- Stateful functions
----------------------------------
showGameGrid :: UiState -> Game -> State Grid ()
showGameGrid ui game = do
    -- Show units
    mapM_ (showUnit ui) (game ^. G.units)

showUnit :: UiState -> Unit -> State Grid ()
showUnit ui unit = do
    let Location (x, y) = unit ^. U.location
        attr =
            if isUnitSelected unit ui
                then defAttr `withForeColor` red
                else defAttr
    ix y . ix x .= ('@', attr)
