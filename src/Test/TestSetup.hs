module Test.TestSetup (
    -- Marquis Setup
    setupCat,
    setupCatDef,
    placeWood,
    -- Mocking
    replaceCardLookup,
) where

import qualified Data.List as Lst
import Data.Maybe

import Lens.Micro
import Lens.Micro.Mtl

import ExecAction
import qualified Root.Game as Game
import Root.IxTable
import Root.Types

----------------------------------
-- Marquis Setup
----------------------------------
setupCat :: (Int, Int, Int, Int) -> Update Game ()
setupCat (keep, sawmill, workshop, recruiter) = do
    let clearingIxs = (keep, sawmill, workshop, recruiter) & each %~ makeIx
    let setupAction = SetupAction $ CatSetupAction clearingIxs
    execAction setupAction

setupCatDef :: Update Game ()
setupCatDef = setupCat (1, 2, 4, 7)

placeWood :: Update Game ()
placeWood = do
    let placeWoodAction = TurnAction $ MarquisAction CatPlaceWood
    execAction placeWoodAction

----------------------------------
-- Mocking
----------------------------------
replaceCardLookup :: [ConIx Card] -> Update Game [Index Card]
replaceCardLookup cardConstructors = do
    let cards = zipWith (\i c -> c (makeIx i)) [0 ..] cardConstructors
    Game.cardLookup .= \i -> fromMaybe def $ Lst.find ((== i) . getIx) cards
    return $ map getIx cards

----------------------------------
-- Misc
----------------------------------
