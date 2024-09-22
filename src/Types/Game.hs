{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Types.Game (
    -- Type
    Game,
    -- Constructor
    newGame
) where

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Types.Alias
import Types.Error
import Types.IxTable as I
import Util

----------------------------------
-- Type
----------------------------------
data Game = Game
    { 
    }
    deriving (Show)

----------------------------------
-- Constructor
----------------------------------
newGame :: Game
newGame = Game

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Game

----------------------------------
-- Stateful Functions
----------------------------------

----------------------------------
-- Unit
----------------------------------

----------------------------------
-- Map
----------------------------------

----------------------------------
-- City
----------------------------------

----------------------------------
-- General
----------------------------------
addIxEntry :: (Id i) => Lens' Game (IxTable a) -> Con i a -> Update Game a
addIxEntry l con = do
    table <- use l
    let (table', x) = I.insert con table
    l .= table'
    return x

getIxEntry :: (Id i) =>  Error -> Lens' Game (IxTable a) -> i -> Update Game a
getIxEntry err l entryId = useEither err $ l . atTable entryId

deleteIxEntry :: (Id i) => Lens' Game (IxTable a) -> i -> Update Game ()
deleteIxEntry l entryId = l %= I.delete entryId
