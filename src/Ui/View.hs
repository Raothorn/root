{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
module Ui.View (
    showGame,
) where

import Lens.Micro

import Graphics.Vty hiding (char, string)
import qualified Graphics.Vty as V

import Types
import qualified Types.Game as G
import qualified Types.IxTable as I
import qualified Types.Location as L
import qualified Types.Unit as U

showGame :: Game -> V.Picture
showGame game =
    V.picForLayers $ reverse $
        mempty
            <> pure showBg
            <> showCities game
            <> showUnits game

showEntity :: Char -> Location -> Image
showEntity char loc = translate (loc ^. L.x) (loc ^. L.y) entityView
  where
    entityView = V.char defAttr char

showBg :: Image
showBg = charFill defAttr '.' w h
  where
    (w, h) = (50, 25) :: (Int, Int)

showUnits :: Game -> [Image]
showUnits game = map (showEntity '@') unitLocs
  where
    unitLocs = game ^. G.units . to I.values ^.. each . U.location

showCities :: Game -> [Image]
showCities game = map (showEntity 'C') cityLocs
    where 
        cityLocs = game ^. G.cities ^.. each
