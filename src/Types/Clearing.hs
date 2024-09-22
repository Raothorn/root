{-# LANGUAGE TemplateHaskell #-}

module Types.Clearing (
    -- Types
    Clearing,
    -- Constructor
    -- Lenses
    suit,
) where

import Data.Coerce (coerce)

import Lens.Micro.TH

import Types
import Types.Common

----------------------------------
-- Types
----------------------------------
data Clearing = Clearing
    { _suit :: Suit
    , _slots :: [Slot]
    }

type Slot = ()

----------------------------------
-- Instances
----------------------------------

----------------------------------
-- Lenses
----------------------------------
makeLenses ''Clearing
