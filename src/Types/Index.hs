{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Types.Index (
    Indexed (..),
    ConIx,
    Index,
    IxList,
    getIx',
    -- Lenses
    ixList,
    -- COnstructors
    makeIx,
    empty,
    createN,
    -- Lookup
    findIndex,
) where

import Data.Coerce

import qualified Data.List as Lst

import Lens.Micro
import Lens.Micro.GHC ()
import Types.Default

----------------------------------
-- Index Type
----------------------------------
newtype Index a = Index Int
    deriving (Eq, Ord)

instance Show (Index a) where
    show (Index n) = show n

instance Default (Index a) where
    def = Index 0

----------------------------------
-- Typeclasses
----------------------------------
class Indexed a where
    getIx :: a -> Index a

getIx' :: (Indexed a) => a -> Int
getIx' x =
    let (Index n) = getIx x
    in  n

----------------------------------
-- Types
----------------------------------
type ConIx a = Index a -> a

newtype IxList a = IxList [a]
    deriving (Functor, Foldable, Traversable)

----------------------------------
-- Lenses
----------------------------------
ixList :: (Indexed a) => Index a -> Traversal' (IxList a) a
ixList index = traversed . filtered (\x -> getIx x == index)

----------------------------------
-- Constructors
----------------------------------
makeIx :: Int -> Index a
makeIx = Index

empty :: IxList a
empty = IxList []

createN :: (Indexed a) => ConIx a -> Int -> Int -> IxList a
createN construct n m = foldr (insert . construct . makeIx) empty [n .. n + m]

----------------------------------
-- Insertion
----------------------------------
insert :: (Indexed a) => a -> IxList a -> IxList a
insert x xs =
    if any (\y -> getIx y == getIx x) xs
        then xs
        else IxList (x : coerce xs)

----------------------------------
-- Lookup
----------------------------------
findIndex :: (Indexed a) => Index a -> [a] -> Maybe a
findIndex i = Lst.find (\x -> getIx x == i)
