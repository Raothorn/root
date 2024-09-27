module Test.TestSetup (
    replaceCardLookup,
    setupCat,
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
setupCat :: Int -> Int -> Int -> Int -> Update Game ()
setupCat keep sawmill workshop recruiter = do
    let clearingIxs = (keep, sawmill, workshop, recruiter) & each %~ makeIx
    let setupAction = SetupAction $ CatSetupAction clearingIxs
    execAction setupAction

----------------------------------
-- Mocking
----------------------------------
replaceCardLookup :: [ConIx Card] -> Update Game [Card]
replaceCardLookup cardConstructors = do
    let cards = zipWith (\i c -> c (makeIx i)) [0 ..] cardConstructors
    Game.cardLookup .= \i -> fromMaybe def $ Lst.find ((== i) . getIx) cards
    return cards
