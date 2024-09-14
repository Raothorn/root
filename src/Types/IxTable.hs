{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module Types.IxTable (
    IxTable,
    empty,
    insert,
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
-- Types
----------------------------------
data Entry a = Entry
    { _isDeleted :: Bool
    , _value :: a
    }

data IxTable a = IxTable
    { _counter :: Int
    , _contents :: M.Map Int (Entry a)
    }

makeLenses ''IxTable
makeLenses ''Entry

----------------------------------
-- Constructors
----------------------------------
empty :: IxTable a
empty = IxTable 0 M.empty

----------------------------------
-- Insertion
----------------------------------
insert :: (Int -> a) -> IxTable a -> (IxTable a, a)
insert construct table = (table', val)
  where
    curCounter = table ^. counter
    val = construct curCounter
    entry = Entry False val
    contents' = M.insert curCounter entry (table ^. contents)
    table' = IxTable (curCounter + 1) contents'

----------------------------------
-- Indexing
----------------------------------
lookup :: Int -> IxTable a -> Maybe a
lookup n table = do
    entry <- M.lookup n (table ^. contents)
    if entry ^. isDeleted
        then Nothing
        else Just (entry ^. value)

-- Lensy getter
atTable :: Int -> SimpleGetter (IxTable a) (Maybe a)
atTable = to . lookup

-- Traversal (for setting)
ixTable :: Int -> Traversal' (IxTable a) a
ixTable n = contents . ix n . value

----------------------------------
-- Deletion
----------------------------------
delete :: Int -> IxTable a -> IxTable a
delete n table = table & contents %~ M.adjust deleteEntry n
  where
    deleteEntry x = x & isDeleted .~ True

----------------------------------
-- Transformations
----------------------------------
values :: IxTable a -> [a]
values = map _value . M.elems . _contents

----------------------------------
-- Instances
----------------------------------
instance Foldable IxTable where
    foldr f x table = foldr f x (values table)

instance (Show a) => Show (IxTable a) where
    show table = show (values table)
