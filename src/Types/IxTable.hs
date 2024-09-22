{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Types.IxTable (
    IxTable (),
    Indexed (..),
    ConIx,
    Index,
    empty,
    insert,
    insert',
    lookup,
    atTable,
    ixTable,
    delete,
    values,
) where

import qualified Data.Map as M
import Prelude hiding (lookup)

import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.TH

----------------------------------
-- Typeclasses
----------------------------------
class Indexed a where
    getIx :: a -> Index a

type ConIx a = Index a -> a

newtype Index a = Index Int
    deriving (Eq, Ord)

----------------------------------
-- Types
----------------------------------
data IxTable a = IxTable
    { _counter :: Int
    , _contents :: M.Map (Index a) a
    }

----------------------------------
-- Instances
----------------------------------
values :: IxTable a -> [a]
values = M.elems . _contents

makeLenses ''IxTable

----------------------------------
-- Constructors
----------------------------------
empty :: IxTable a
empty = IxTable 0 M.empty

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
