{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.IxTable (
    IxTable,
    Indexed (..),
    ConIx,
    Index,
    getIx',
    makeIx,
    empty,
    insert,
    insert',
    lookup,
    atTable,
    ixTable,
    delete,
    traverseTable,
    values,
    createN,
) where

import qualified Data.Map as M
import Prelude hiding (lookup)

import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.TH

import Types.Default

----------------------------------
-- Typeclasses
----------------------------------
class Indexed a where
    getIx :: a -> Index a

getIx' :: (Indexed a) => a -> Int
getIx' x =
    let (Index n) = getIx x
    in  n

type ConIx a = Index a -> a

----------------------------------
-- Types
----------------------------------
data IxTable a = IxTable
    { _counter :: Int
    , _contents :: M.Map (Index a) a
    }

newtype Index a = Index Int
    deriving (Eq, Ord)

makeLenses ''IxTable

----------------------------------
-- Instances
----------------------------------
instance Show (Index a) where
    show (Index n) = show n

instance Default (Index a) where
    def = Index 0

instance Functor IxTable where
    fmap f tbl = tbl & contents %~ M.mapKeys changeIndex . fmap f

instance Foldable IxTable where
    foldr :: (a -> b -> b) -> b -> IxTable a -> b
    foldr f x tbl = foldr f x (tbl ^. contents)

----------------------------------
-- Constructors
----------------------------------
makeIx :: Int -> Index a
makeIx = Index

empty :: IxTable a
empty = IxTable 0 M.empty

createN :: ConIx a -> Int -> Int -> IxTable a
createN constructor startIx n = foldr (\_ t -> insert' constructor t) initTable [1 .. n]
  where
    initTable = empty & counter .~ startIx

----------------------------------
-- Insertion
----------------------------------
insert :: ConIx a -> IxTable a -> (IxTable a, a)
insert construct table = (table', val)
  where
    curCounter = Index (table ^. counter)
    val = construct curCounter
    contents' = M.insert curCounter val (table ^. contents)
    table' = IxTable (table ^. counter + 1) contents'

insert' :: ConIx a -> IxTable a -> IxTable a
insert' c t = fst $ insert c t

----------------------------------
-- Indexing
----------------------------------
lookup :: Index a -> IxTable a -> Maybe a
lookup i table = M.lookup i (table ^. contents)

-- Lensy getter
atTable :: Index a -> SimpleGetter (IxTable a) (Maybe a)
atTable = to . lookup

-- Traversal (for setting)
ixTable :: Index a -> Traversal' (IxTable a) a
ixTable i = contents . ix i

----------------------------------
-- Deletion
----------------------------------
delete :: Index a -> IxTable a -> IxTable a
delete x table = table & contents %~ M.delete x

----------------------------------
-- Lenses
----------------------------------
traverseTable :: Traversal' (IxTable a) a
traverseTable = contents . traversed

----------------------------------
-- Transformers
----------------------------------

values :: IxTable a -> [a]
values = M.elems . _contents

changeIndex :: Index a -> Index b
changeIndex (Index n) = Index n
