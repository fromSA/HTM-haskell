{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}


--{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

-- |
-- Module      : SDR
-- Description : An data type representing an SDR
-- Copyright   : (c) Fromsa Hera, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module represents an 'SDR' as defined by (Numenta).
-- See [Encoding Data for HTM Systems](https://arxiv.org/pdf/1602.05925.pdf) for detailed explanation.
module SRC.SDR where

import Control.Lens (makeLenses)
import SRC.CommonDataTypes (BitIndex)
import GHC.Generics ( Generic ) 

-- | A data type for the possible range of an SDR value.
data SDRRange = SDRRange
  { -- | The smallest sdr bit index. TODO remove this, because it is implisitly = 0
    _minIndex :: BitIndex,
    -- | The largest sdr bit index.
    _maxIndex :: BitIndex
  }
  deriving (Show, Generic)

makeLenses ''SDRRange

-- | The range of an SDR.
data SDR = SDR
  { -- | A list of bit indecies. Used as a representaion of an input value, like an integer, class or other data type.
    --  The values are transformed to SDR using an encoder
    _sdr :: [BitIndex],
    _sdrRange :: SDRRange
  }
  deriving (Show)

makeLenses ''SDR
