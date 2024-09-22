{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Game (
    -- Type
    Game,
    -- Lenses
    gameLog,
    phase,
    board,
    -- Constructor
    --Stateful functions
    zoomClearing,
    zoomClearing',
    zoomCat
) where

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import Types.IxTable
import Types.Alias
import Types.RootBoard
import Types.Faction
import Types.LogEvent
import Types.Phase
import Types.Clearing (Clearing)
import qualified Types.Clearing as Clr

----------------------------------
-- Type
----------------------------------
data Game = Game
    { _gameLog :: [LogEvent]
    , _phase :: Phase
    , _board :: RootBoard
    , _playerFactions :: CatFaction
    }
    deriving (Show)

----------------------------------
-- Constructor
----------------------------------

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Game

----------------------------------
-- Stateful Functions
----------------------------------
zoomClearing :: (Monoid a) => Index Clearing -> Update Clearing a -> Update Game a
zoomClearing i = zoom (board . clearings . ixTable i)

zoomClearing' :: (Monoid a) => Clearing -> Update Clearing a -> Update Game a
zoomClearing' = zoomClearing . getIx

zoomCat :: Update CatFaction a -> Update Game a
zoomCat = zoom playerFactions
