{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Game (
    -- Type
    Game,
    -- Lenses
    gameLog,
    board,
    -- Stateful functions
    -- Zooming
    zoomClearing,
    zoomClearing',
    zoomCat,
    -- Logging
    logEvent,
    -- Phase
    getPhase,
    setPhase,
    pushPhase,
    popPhase,
) where

import Data.Maybe

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Types.Alias
import Types.Clearing (Clearing)
import Types.Default
import Types.Faction
import Types.IxTable
import Types.LogEvent
import Types.Phase
import Types.RootBoard

----------------------------------
-- Type
----------------------------------
data Game = Game
    { _gameLog :: [LogEvent]
    , _phaseStack :: [Phase]
    , _board :: RootBoard
    , _playerFactions :: PlayerFactions
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
            { _gameLog = []
            , _phaseStack = [def]
            , _board = def
            , _playerFactions = def
            }

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Game
makeLenses ''PlayerFactions

----------------------------------
-- Stateful Functions
----------------------------------
----------------------------------
-- Zooming
----------------------------------
zoomClearing :: (Monoid a) => Index Clearing -> Update Clearing a -> Update Game a
zoomClearing i = zoom (board . clearings . ixTable i)

zoomClearing' :: (Monoid a) => Clearing -> Update Clearing a -> Update Game a
zoomClearing' = zoomClearing . getIx

zoomCat :: (Monoid a) => Update CatFaction a -> Update Game a
zoomCat = zoom (playerFactions . marquis . traversed)

zoomBird :: (Monoid a) => Update BirdFaction a -> Update Game a
zoomBird = zoom (playerFactions . eerie . traversed)

----------------------------------
-- Logging
----------------------------------
logEvent :: LogEvent -> Update Game ()
logEvent event = gameLog %= (++ [event])

----------------------------------
-- Phase
----------------------------------
-- Under normal operations, the phase stack should never become empty.
-- It is on us as the developer to make sure that pushes and pops
-- happen in pairs. The easiest way to do this is to use "setPhase"
-- whenever possible, instead of pushPhase and popPhase
getPhase :: Update Game Phase
getPhase = preuse (phaseStack . _head) <&> fromMaybe NoPhase

setPhase :: Phase -> Update Game ()
setPhase phase = popPhase >> pushPhase phase

pushPhase :: Phase -> Update Game ()
pushPhase phase = phaseStack %= (phase :)

popPhase :: Update Game ()
popPhase = phaseStack %= tail
