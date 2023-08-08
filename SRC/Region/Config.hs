{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Config
-- Description : A configuration module.
-- Copyright   : (c) Fromsa Hera, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the configuration parameterer for the construction of a Region model.
module SRC.Region.Config where

import Control.Lens (makeLenses)
import GHC.Natural (Natural)

-- | Mapper between SDR and Region
data MappingType
  = -- | Connect a column in a Region to constant subset of SDR bit indecies selected randomly.
    Random
  deriving (Show)

-- -------------------------------------------------------------
--                           CONFIG
-- -------------------------------------------------------------

-- | The configuration parameters for a Region.
data RegionConfig = RegionConfig
  { -- | The number of columns in the region.
    _nrOfColumns :: Natural,
    -- | The number of cells per column in the region. It is the same for all cells.
    _nrOfCellsPerColumn :: Natural,
    -- | The maximum number of input bits in the inputfield connected to a Column.
    _initNrOfFeedForwardSynpases :: Natural,
    -- | The number of initial synapses per segment.
    _nrOfSynapsesPerSegment :: Natural,
    -- | The mapping type between an input sdr and region columns -- TODO delete
    _mappingType :: MappingType,
    -- | The connection strength of synapses between cells in a region.
    _initConnectionStrength :: Float,
    -- | The size of the window of the moving average, used in overlap and active duty cycle.
    _mvWindow :: Natural,
    -- | A constant value, represents the initial radius of a columns neighbourhood.
    _initRad :: Natural

  } deriving (Show)

makeLenses ''RegionConfig

defualtRegionConfig :: RegionConfig
defualtRegionConfig =
  RegionConfig
    { _nrOfColumns = 10,
      _nrOfCellsPerColumn = 4,
      _initNrOfFeedForwardSynpases = 8,
      _nrOfSynapsesPerSegment = 10,
      _mappingType = Random,
      _initConnectionStrength = 0.6,
      _mvWindow = 5,
      _initRad = 5
    }