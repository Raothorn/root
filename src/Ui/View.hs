module Ui.View (
    showGame,
) where

import Lens.Micro

import Graphics.Vty

import Types

showGame :: Game -> Picture
showGame game =
    picForLayers $
        mempty
            ++ [unitView]
            ++ citiesView
            ++ [bgView]
  where
    (gameWidth, gameHeight) = (50, 25) :: (Int, Int)

    bgView = charFill defAttr '.' gameWidth gameHeight
    unitLoc = game ^. unit . location
    unitView = translate (unitLoc ^. x) (unitLoc ^. y) (char defAttr '@')
    citiesView = showCities game

showCities :: Game -> [Image]
showCities game = map showCity (game ^. cities)
  where
    showCity city = translate (city ^. x) (city ^. y) (char defAttr 'C')
