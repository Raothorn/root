{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module Types.IxTable (
    IxTable,
    Id(..),
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

import Types.Alias

----------------------------------
-- Typeclasses
----------------------------------
-- types that are isomorphic to integers
class Id a where
    toInt :: a -> Int
    fromInt :: Int -> a

----------------------------------
-- Types
----------------------------------
data Entry a = Entry
    { _isDeleted :: Bool
    , _value :: a
    }
    deriving (Traversable)

data IxTable a = IxTable
    { _counter :: Int
    , _contents :: M.Map Int (Entry a)
    }
    deriving (Traversable)

----------------------------------
-- Instances
----------------------------------
instance Functor Entry where
    fmap f entry = entry { _value = f (_value entry)}

instance Foldable Entry where
    foldr f x entry = f (_value entry) x

instance Functor IxTable where
    fmap f table = table { _contents = contents' }
        where 
            contents' = fmap (fmap f) (_contents table)

instance Foldable IxTable where
    foldr f x table = foldr f x (map _value $ M.elems (_contents table))


instance (Show a) => Show (IxTable a) where
    show table = show (map _value $ M.elems (_contents table))

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
insert :: (Id i) => Con i a -> IxTable a -> (IxTable a, a)
insert construct table = (table', val)
  where
    curCounter = table ^. counter
    val = construct (fromInt curCounter)
    entry = Entry False val
    contents' = M.insert curCounter entry (table ^. contents)
    table' = IxTable (curCounter + 1) contents'

----------------------------------
-- Indexing
----------------------------------
lookup :: (Id i) => i -> IxTable a -> Maybe a
lookup n table = do
    entry <- M.lookup (toInt n) (table ^. contents)
    if entry ^. isDeleted
        then Nothing
        else Just (entry ^. value)

-- Lensy getter
atTable :: (Id i) => i -> SimpleGetter (IxTable a) (Maybe a)
atTable = to . lookup

-- Traversal (for setting)
ixTable :: (Id i) => i -> Traversal' (IxTable a) a
ixTable n = contents . ix (toInt n) . value

----------------------------------
-- Deletion
----------------------------------
delete :: (Id i) => i -> IxTable a -> IxTable a
delete n table = table & contents %~ M.adjust deleteEntry (toInt n)
  where
    deleteEntry x = x & isDeleted .~ True

----------------------------------
-- Transformations
----------------------------------
values :: IxTable a -> [a]
values = map _value . M.elems . _contents

