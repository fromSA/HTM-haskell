{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module SRC.Package where

import SRC.HTM.Config
import SRC.Region.Config
import SRC.Encoder.Config
import SRC.SDR (SDR)
import Control.Lens
import System.Random (StdGen)

-- | A package record containing the configaration parameters and the sdr for the current time step. This is used for the simplication of the function types.
data Package = Package
  { -- | The configuration parameters for the HTM algorithm.
    _conH :: HTMConfig,
    -- | The configuration parameters for a Region.
    _conR :: RegionConfig,
    -- | The configuration parameters for the SDR encoding.
    _conS :: EncoderConfig,
    -- | The SDR encoding of the current value.
    _value :: SDR,
    -- | And StdGen for generating random values. Should always be updated after use.
    randomGenerator :: StdGen
  }

-- | Lenses for HTMConfig, used to navigate the record.
makeLenses ''Package