{-# LANGUAGE TemplateHaskell #-}
module Types.Clearing (
    --Type
    Clearing,
    -- Constructor
    -- Lenses
    suit
) where

import Lens.Micro.TH

import Types.Common

----------------------------------
-- Type
----------------------------------
data Clearing = Clearing
    { _suit :: Suit
    , _slots :: [Slot]
    }

type Slot = ()
----------------------------------
-- Lenses
----------------------------------
makeLenses ''Clearing
