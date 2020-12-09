{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module SRC.HTM.Config where

import Control.Lens
import GHC.Natural (Natural)

-- -------------------------------------------------------------
--                           CONFIG
-- -------------------------------------------------------------

-- | The parameters for the feedforward synapses that connect the inputField with a region and spatial algorithm.
data SpatialConfig = SpatialConfig
  { -- | The threshold for column activation. If the number of active bits in the inputfield of a column >= this threshold, then the column becomes active.
    _overlapThreshold :: Natural,
    -- | The minimum percent of active bits in inputField expected to have a overlap with a column.
    _mop :: Float,
    -- | The amount to decrease the connection strength of a synapses with for proximal synapses
    _proxSynConInc :: Float,
    -- | The amount to decrease the connection strength of a synapses with for proximal synapses
    _proxSynConDec :: Float,
    -- | A synapse with connection strength >= this threshold is considered permenantly connected.
    _pConthresh :: Float,
    -- | The desired column activity level within inhibition radius i.e. the number of columns that should be activated within the inhibition radius.
    _colActLev :: Natural
  }

makeLenses ''SpatialConfig

-- | The parameters for the temporal algorithm.
data TemporalConfig = TemporalConfig
  { -- | The degree of change for the update of '_boost'. It is between 0 and 1.
    _boostStrength :: Float,
    -- | The desired percent of active duty cycle within the sliding window.
    _targetDensity :: Float,
    -- | The punishment strength, used to punish wrongly predicted segments.
    _predictedDecrement :: Float,
    -- | The forgetting strength, detrmines how fast a pattern is forgotten by the region.
    _permanenceDecrement :: Float,
    -- | The learning strength, determines how fast a pattern is learned by the region
    _permanenceIncrement :: Float,
    -- | The threshold of permenant synaptic connection. If a synapse has higher connection than this threshold and is connected to an active cell, then it is considered active.
    _connectedPermenance :: Float,
    -- | The threshold for when a cell is considered matching, i.e. when a segment has this many activeconnections to the previously active/winner cells.
    _learningThreshold :: Natural,
    -- | The number of synapses per segment that must be active for the segment to be considered active. should be less than nrOfSynapsesPerSegment of RegionConfig
    _activationThreshold :: Natural,
    -- | True if the temporalPooler should update the connectionstrengths of the synapses.
    _learningEnabled :: Bool
  }

makeLenses ''TemporalConfig

-- | The configuration parameters for the HTM algorithm.
data HTMConfig = HTMConfig
  { -- | see 'SpatialConfig'
    _spatialConfig :: SpatialConfig,
    -- | see 'TemporalConfig'
    _temporalConfig :: TemporalConfig
  }

-- | Lenses for HTMConfig, used to navigate the record.
makeLenses ''HTMConfig

defualtHTMConfig :: HTMConfig
defualtHTMConfig =
  HTMConfig
    { _spatialConfig = defualtSpatialConfig,
      _temporalConfig = defualtTemporalConfig
    }

defualtTemporalConfig :: TemporalConfig
defualtTemporalConfig =
  TemporalConfig
    { _boostStrength = 0.5,
      _targetDensity = 0.6,
      _predictedDecrement = 0.1,
      _permanenceDecrement = 0.05,
      _permanenceIncrement = 0.07,
      _connectedPermenance = 0.6,
      _learningThreshold = 5,
      _activationThreshold = 7,
      _learningEnabled = True
    }

defualtSpatialConfig :: SpatialConfig
defualtSpatialConfig =
  SpatialConfig
    { _overlapThreshold = 5,
      _mop = 0.4,
      _proxSynConInc = 0.09,
      _proxSynConDec = 0.06,
      _pConthresh = 0.6,
      _colActLev = 3
    }