{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : SRC.Package
-- Description : A record representing the configuration parameters and the inputdata
-- Copyright   : (c) Fromsa Hera, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains a Package record that used by the HTM, Region and Encoder modules.
module SRC.Package where

import SRC.HTM.Config
import SRC.Region.Config
import SRC.Encoder.Config
import SRC.SDR (SDR, SDRRange)
import Control.Lens
import System.Random (StdGen)

-- | A package record containing the configaration parameters and the sdr for the current time step. This is used for the simplication of the function types.
data Package = Package
  { -- | The configuration parameters for the HTM algorithm.
    _conH :: HTMConfig,
    -- | The configuration parameters for a Region.
    _conR :: RegionConfig,
    -- | The configuration parameters for the SDR encoding.
    _conS :: Maybe SDRRange,
    -- | The SDR encoding of the current value.
    _value :: SDR,
    -- | And StdGen for generating random values. Should always be updated after use. For future update.
    _randomGenerator :: StdGen
  } deriving (Show)

-- | Lenses for HTMConfig, used to navigate the record.
makeLenses ''Package