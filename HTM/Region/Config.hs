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
module HTM.Region.Config
  ( MappingType (..),
    RegionConfig (..),
    nrOfCellsPerColumn,
    nrOfColumns,
    nrOfSynapsesPerSegment,
    mappingType,
    initConnectionStrength,
    maxNrOfInputBits,
    mvWindow,
  )
where

import Control.Lens (makeLenses)
import GHC.Natural ( Natural )

-- | Mapper between SDR and Region
data MappingType = 
    -- | Connect a column in a Region to constant subset of SDR bit indecies selected randomly.
    Random

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
    _maxNrOfInputBits :: Natural,
    -- | The number of initial synapses per segment.
    _nrOfSynapsesPerSegment :: Natural,
    -- | The mapping type between an input sdr and region columns -- TODO delete
    _mappingType :: MappingType,
    -- | The connection strength of synapses between cells in a region.
    _initConnectionStrength :: Float,
    -- | The size of the window of the moving average, used in overlap and active duty cycle.
    _mvWindow :: Natural
  }

makeLenses ''RegionConfig