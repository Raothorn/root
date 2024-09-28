{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Game (
    -- Type
    Game,
    -- Lenses
    phaseStack,
    board,
    playerFactions,
    marquis,
    eerie,
    factionsInPlay,
    cardLookup,
    catFaction,
    birdFaction,
    factionCommon,
    clearingAt,
    allClearingIxs,
    traverseClearings,
    -- Constructors
    newForestGame,
) where

import Data.Maybe

import Lens.Micro
import Lens.Micro.TH

import Lookup.BoardLookup
import Lookup.CardLookup
import Types.Board (Board)
import qualified Types.Board as Board
import Types.Card
import Types.Clearing (Clearing)
import Types.Default
import Types.Faction (BirdFaction, CatFaction, Faction (..), FactionCommon)
import qualified Types.Faction.Eerie as Bird
import qualified Types.Faction.Marquis as Cat
import Types.IxTable (Index)
import qualified Types.IxTable as I
import Types.Phase (Phase)

----------------------------------
-- Type
----------------------------------
data Game = Game
    { _phaseStack :: [Phase]
    , _board :: Board
    , _playerFactions :: PlayerFactions
    , -- Swappable lookup functions
      _cardLookup :: Index Card -> Card
    }

data PlayerFactions = PlayerFactions
    { _marquis :: Maybe CatFaction
    , _eerie :: Maybe BirdFaction
    }

----------------------------------
-- Instances
----------------------------------
instance Default PlayerFactions where
    def = PlayerFactions Nothing Nothing

instance Default Game where
    def =
        Game
            { _phaseStack = []
            , _board = def
            , _playerFactions = def
            , _cardLookup = lookupCard
            }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Game
makeLenses ''PlayerFactions

----------------------------------
-- Factions
----------------------------------
factionsInPlay :: SimpleGetter Game [Faction]
factionsInPlay = to factionsInPlay'
  where
    factionsInPlay' game =
        let cat = fmap (const Marquis) $ game ^. playerFactions . marquis
            bird = fmap (const Eerie) $ game ^. playerFactions . eerie
        in  catMaybes [cat, bird]

catFaction :: Traversal' Game CatFaction
catFaction = playerFactions . marquis . _Just

birdFaction :: Traversal' Game BirdFaction
birdFaction = playerFactions . eerie . _Just

----------------------------------
-- FactionCommon
----------------------------------
factionCommon :: Faction -> Traversal' Game FactionCommon
factionCommon Marquis = catFaction . Cat.marquisCommon
factionCommon Eerie = birdFaction . Bird.eerieCommon

----------------------------------
-- Board
----------------------------------
clearingAt :: Index Clearing -> Traversal' Game Clearing
clearingAt i = board . Board.clearings . I.ixTable i

allClearingIxs :: SimpleGetter Game [Index Clearing]
allClearingIxs = board . Board.clearings . to I.values . to (map I.getIx)

traverseClearings :: Traversal' Game Clearing
traverseClearings = board . Board.clearings . I.traverseTable

----------------------------------
-- Constructors
----------------------------------
newForestGame :: Game
newForestGame =
    def & (board .~ forestMap)
