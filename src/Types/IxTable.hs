module Types.IxTable (
    IxTable,
    empty,
    insert,
    lookup
) where

import Prelude hiding (lookup)
import qualified Data.Map as M

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

----------------------------------
-- Constructors
----------------------------------
empty :: IxTable a
empty = IxTable 0 M.empty

----------------------------------
-- Insertion
----------------------------------
insert :: (Int -> a) -> IxTable a -> (IxTable a, a)
insert construct table = (table', value)
    where 
        counter = _counter table
        value = construct counter
        entry = Entry False value
        contents' = M.insert counter entry (_contents table)
        table' = IxTable (counter + 1) contents'

----------------------------------
-- Indexing
----------------------------------
lookup :: Int -> IxTable a -> Maybe a
lookup n = fmap _value . M.lookup n . _contents

----------------------------------
-- Deletion
----------------------------------
delete :: Int -> IxTable a -> IxTable a
delete n table = table { _contents = contents' } 
    where 
        deleteEntry x = x { _isDeleted = True }
        contents = _contents table     
        contents' = M.adjust deleteEntry n contents 


