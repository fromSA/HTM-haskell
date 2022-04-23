{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Region
-- Description : The datastucture and construction of a region in the HTM algorithm.
-- Copyright   : (c) Fromsa Hera, 2020
--                   Numenta, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module defines the data type of a Region.
-- A Region consists of columns which in turn consists of cells.
-- A cell consists of dendrites which in turn consists of segments.
-- A Segment is a collection of synsapses that recieve input from other cells in the region.
module SRC.Region.Model where

import Control.Lens (makeLenses, (^.))
import Data.List (intercalate)
import Debug.Trace ()
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import SRC.CommonDataTypes (BitIndex, Index')
import SRC.MovingAverage (MovingAverage (..))

import Diagrams.Prelude 
import Diagrams.Backend.SVG
import Diagrams.Size

-- -------------------------------------------------------------
--                           CONSTANTS
-- -------------------------------------------------------------

-- | A constant value, represents what show inplace of an active state.
_ACTIVE_STATE :: String
_ACTIVE_STATE = "1"

-- | A constant value, represents what show inplace of an inactive state.
_INACTIVE_STATE :: String
_INACTIVE_STATE = "0"

-- | A constant value, represents what show inplace of an predictive state.
_PREDICTIVE_STATE :: String
_PREDICTIVE_STATE = "p"

-- | A constant value, represents what show inplace of an active and predictive state.
_ACTIVE_AND_PREDICTIVE_STATE :: String
_ACTIVE_AND_PREDICTIVE_STATE = "a"

-- | A constant value, represents what show inplace of an matching state.
_MATCHING_STATE :: String
_MATCHING_STATE = "m"

-- -------------------------------------------------------------
--                           MODEL
-- -------------------------------------------------------------

-- | Represent the indentity of a dendrite with in the list of dendrites it is contained in.
type DendriteIndex = Index'

-- | Represent the indentity of a segment with in the list of segments it is contained in.
type SegmentIndex = Index'

-- | Represent the indentity of a column with in the list of column it is contained in.
type ColumnIndex = Index'

-- | Represent the indentity of a cell with in the list of cell it is contained in.
type CellIndex = Index'

-- | Represents a synapse connection, a float between 0 and 1
type ConnectionStrength = Float

-- | The two states a column can exist in.
data ColumnState
  = -- | A column is active if it is connected to enought active SDR bits.
    ActiveColumn
  | -- | A column is inactive if it is not connected to enough active SDR bits.
    InActiveColumn
  deriving (Eq, Generic)

instance Show ColumnState where
  show ActiveColumn = _ACTIVE_STATE
  show InActiveColumn = _INACTIVE_STATE

-- | The four states of a cell.
data CellState
  = -- | A cell is active if the column it belongs to is active and the column bursts or this cell was predicted to activate.
    ActiveCell
  | -- | A cell is predictive if it is permenantly connected to enough active cells within the the same region.
    PredictiveCell
  | -- | A cell in this state is both active and predictive at the same time.
    ActivePredictiveCell
  | -- | A cell is inactive if it is neither active nor predicted.
    InActiveCell
  deriving (Eq, Generic)

instance Show CellState where
  show ActiveCell = _ACTIVE_STATE
  show InActiveCell = _INACTIVE_STATE
  show PredictiveCell = _PREDICTIVE_STATE
  show ActivePredictiveCell = _ACTIVE_AND_PREDICTIVE_STATE

-- | The three states of a segment.
data SegmentState
  = -- | A segment is active if enough of its synapses are permenantly connected to active cells, exceedes the 'HTM._activationThreshold'.
    ActiveSegment
  | -- | A segment is matching if the number of its synapses, permenantly connected to active cells, exceedes the 'HTM._learningThreshold'.
    MatchingSegment
  | -- | A segment is inactive if it is neither 'ActiveSegment' nor 'MatchingSegment'.
    InActiveSegment
  deriving (Eq, Generic)

instance Show SegmentState where
  show ActiveSegment = _ACTIVE_STATE
  show InActiveSegment = _INACTIVE_STATE
  show MatchingSegment = _MATCHING_STATE

-- | Represents the id of a cell within a Region. Two cells from two different regions
-- can have the same CellID.
data CellID = CellID
  { -- | The index of a column within a region.
    _col :: ColumnIndex,
    -- | The index of a column within a column. Multiple cells with have the same `_cell` index, bacause it is relative.
    _cell :: CellIndex
  }
  deriving (Eq, Show, Generic)

makeLenses ''CellID

-- | A synapse is the connection between two cells. A synapse is part of a group of synapses called segment.
--  Each segment is attached to a dendrite.
data Synapse = Synapse
  { -- | The cell from which the input is coming.
    _source :: CellID,
    -- | The cell too which the input is going.
    _destination :: CellID,
    -- | The connection strength between the source and destination
    _connectionStrength :: ConnectionStrength
  }
  deriving (Eq, Show, Generic)

makeLenses ''Synapse

-- | A collection of synapses.
data Segment = Segment
  { -- | The state of this segment.
    _segmentState :: SegmentState,
    -- | These synapses together define a segment. Each segment is expected to encode at least one pattern to recognise.
    _synapses :: [Synapse],
    -- | The number of synpases permenantly connected to active cells.
    _matchingStrength :: Natural
  }
  deriving (Eq, Show, Generic)

makeLenses ''SRC.Region.Model.Segment

-- | A collection of segments. A dendrite can come from within the same region or from another region.
type Dendrite = [SRC.Region.Model.Segment]

-- | A data structure representing a neuron
data Cell = Cell
  { -- | A unique id representing a cell within a region.
    _cellId :: CellID,
    -- | A set of dendrites.
    _dendrites :: [Dendrite],
    -- | The state of this cell.
    _cellState :: CellState,
    -- | This cell is a winner if it is choosen to represent the current encoding. i.e.
    -- It was predicted, or it is the cell with the best matching segment or it is the least used cell within a bursting column.
    _isWinner :: Bool
  }
  deriving (Eq, Show, Generic)

makeLenses ''Cell

-- | A synapses connected to the input-SDR.
data FeedForwardSynapse = FeedForwardSynapse
  { -- | The index of the SDR bit that this synapse is connected to.
    _ind :: BitIndex,
    -- | The connection strength of this synapse.
    _conStr :: ConnectionStrength
  }
  deriving (Eq, Generic)

makeLenses ''FeedForwardSynapse

instance Show FeedForwardSynapse where
  show = show . _conStr

-- show = show . _ind

-- | A collection of cells that recieve the same input.
data Column = Column
  { -- | The index of the column with in a region. The columns in a region are just ordered list.
    _columnId :: ColumnIndex,
    -- | The cells in a column. When a column is active, one or more of these cell will represent that state. These cells get the same input from the input-SDR, i.e. the input to this column.
    _cells :: [Cell],
    -- | The bit indecies this columns is connected to in the SDR input space.
    _inputField :: [FeedForwardSynapse],
    -- | The state of this column.
    _columnState :: ColumnState,
    -- | Should be at least 1. If a column has small moving activation average, it is boosted within the inhibition algorithm.
    -- This value is used to enforce sparcity within the region.
    -- If a boost value is one, then the '_overlap' value remains the same.
    -- Else if it is larger or smaller that one, then the '_overlap' value is scaled accordingly.
    _boost :: Float,
    -- | The number of active SDRbits this column is connected to. This value can be boosted by the boost factor.
    _overlap :: Natural,
    -- | The radius of inhibition i.e. the inhibition algorithm looks at the neighbors of this column, this value determines range of the neighbor.
    _inhibRad :: Natural,
    -- | Active duty cylce. The moving Average rate of how often this column is activated.
    _adc :: MovingAverage,
    -- | Overlap duty cycle. The moving average rate of how often this column had bigger overlap with input field than the activation threshold
    _odc :: MovingAverage
  }
  deriving (Generic)

makeLenses ''Column

-- | A region is a set of columns, The HTM algorithm needs to look at previously active cells to predict the next active cells.
--  Therefore a region is represented with to versions of each columns, the current and previous time step of the region.
data Region = Region
  { -- | The columns in the current time step, used by the temporal algorithm of the HTM algorithm.
    _currentStep :: [Column],
    -- | The columns in the previous time step, used by the temporal algorithm of the HTM algorthm.
    _previousStep :: [Column]
  }
  deriving (Generic)

makeLenses ''Region

instance Eq Column where
  c1 == c2 = c1 ^. inputField == c2 ^. inputField

-- -------------------------------------------------------------
--                           VIEW
-- -------------------------------------------------------------

instance Show Region where
  show r = "C: " ++ show (r ^. currentStep) ++ "\nP: " ++ show (r ^. previousStep)

instance Show Column where
  -- show = show . _overlap
  -- show column = intercalate "" $ map show $ _cells column
  -- show = show . _columnState
  -- show = show . _inputField
  -- show = show . _adc
  show = intercalate "" . map show . _cells

-- show = show . _boost

-- instance Show Cell where
-- show = show . _cellState
-- show cell = (show (fromEnum $ _isWinner cell)) ++  (show (_dendrites cell))
-- show = show . fromEnum . _isWinner
-- show = show . _dendrites

-- instance Show Segment where
-- show = show . _segmentState
-- show = show . length . _synapses

-- instance Show Synapse where
-- show = show . _destination

-- instance Show CellID where
-- show = show . _col

---- To SVG

renderRegion :: Region -> Diagram B
renderRegion r = let x:xs =  map renderColumn (r^.currentStep) in
                    foldl (|||) x xs

renderColumn :: Column -> Diagram B
renderColumn c = let x:xs =  map renderCell (c^.cells) in
                    foldl (|||) x xs

renderCell :: Cell -> Diagram B
renderCell c = renderCellState (c^.cellState) 
                  

renderCellState :: CellState -> Diagram B
renderCellState ActiveCell = visualCell 1 green
renderCellState PredictiveCell = visualCell 1 yellow
renderCellState ActivePredictiveCell = visualCell 1 red
renderCellState InActiveCell = visualCell 1 white

visualCell :: Double -> Colour Double -> Diagram B
visualCell x c = square x # fc c
                      # lw ultraThin
                      # lc black