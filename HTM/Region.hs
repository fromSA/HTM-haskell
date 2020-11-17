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
-- Here is a longer description of this module, containing some
-- commentary with @some markup@.
module HTM.Region where

import Control.Lens ( (^.), makeLenses )
import Data.List (intercalate)
import Debug.Trace ()
import GHC.Natural ( Natural, intToNatural, naturalToInt )
import HTM.CommonDataTypes ( BitIndex, Index' )
import HTM.MovingAverage ( MovingAverage(..) )
import HTM.SDR
    ( SDRRange, SDRConfig, maxIndex, minIndex, sdrRange )
import System.Random ( getStdRandom, Random(randomR) )

-- -------------------------------------------------------------
--                           CONFIG
-- -------------------------------------------------------------

-- | Mapper between SDR and Region
data MappingType = Random

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

-- -------------------------------------------------------------
--                           MODEL
-- -------------------------------------------------------------

type DendriteIndex =
  -- | unsigned int
  Index'

type SegmentIndex =
  -- | unsigned int
  Index'

type ColumnIndex =
  -- | unsigned int
  Index'

type CellIndex =
  -- | unsigned int
  Index'

type ConnectionStrength =
  -- | Between 0 and 1
  Float

-- | The two states a column can exist in.
data ColumnState
  = -- | A column is active if it is connected to enought active SDR bits.
    ActiveColumn
  | -- | A column is inactive if it is not connected to enough active SDR bits.
    InActiveColumn
  deriving (Eq)

-- | The three states a cell can exist in.
data CellState
  = -- | A cell is active if the column it belongs to is active and the column bursts or this cell was predicted to activate.
    ActiveCell
  | -- | A cell is predictive if it is permenantly connected to enough active cells within the the same region.
    PredictiveCell
  | -- | A cell in this state is both active and predictive at the same time.
    ActivePredictiveCell
  | -- | A cell is inactive if it is neither active nor predicted.
    InActiveCell
  deriving (Eq)

-- | The four states of a segment.
data SegmentState
  = -- | A segment is active if enough of its synapses are permenantly connected to active cells, exceedes the `ActivationThreshold`.
    ActiveSegment
  | -- | A segment is matching if the number of its synapses, permenantly connected to active cells, exceedes the `LearningThreshold`.
    MatchingSegment
  | -- | A segment is inactive if it is neither active nor matching.
    InActiveSegment
  deriving (Eq)

-- | Represents the id of a cell within a Region. Two cells from two different regions
-- can have the same CellID.
data CellID = CellID
  { -- | The index of a column within a region.
    _col :: ColumnIndex,
    -- | The index of a column within a column. Multiple cells with have the same `_cell` index, bacause it is relative.
    _cell :: CellIndex
  }
  deriving (Eq)

makeLenses ''CellID

-- | A synapse is the connection between two cells. A synapse is part of a group of synapses called segment.
--  Each segment is attached to a dendrite.
data Synapse = Synapse
  { -- | The cell too which the input is going.
    _source :: CellID,
    -- | The cell from which the input is coming.
    _destination :: CellID,
    -- | The connection strength between the source and destination
    _connectionStrength :: ConnectionStrength
  }
  deriving (Eq)

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
  deriving (Eq)

makeLenses ''Segment

-- | A collection of segments. A dendrite can come from within the same region or from another region.
type Dendrite = [Segment]

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
  deriving (Eq)

makeLenses ''Cell

-- | A tuple containing a bit index in the inputSDR along with a connecition strength
data FeedForwardSynapse = FeedForwardSynapse
  { -- | The index of the SDR bit that this synapse is connected to.
    _ind :: BitIndex,
    -- | The connection strength of this synapse.
    _conStr :: ConnectionStrength
  }
  deriving (Eq)

makeLenses ''FeedForwardSynapse

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
    -- If a boost value is one, then the `_overlap` value remains the same.
    -- Else if it is larger or smaller that one, then the `_overlap` value is scaled accordingly.
    _boost :: Float,
    -- | The number of active SDRbits this column is connected to. This value can be boosted by the boost factor.
    _overlap :: Int,
    -- | The radius of inhibition i.e. the inhibition algorithm looks at the neighbors of this column, this value determines range of the neighbor.
    _inhibRad :: Natural,
    -- | Active duty cylce. The moving Average rate of how often this column is activated.
    _adc :: MovingAverage,
    -- | Overlap duty cycle. The moving average rate of how often this column had bigger overlap with input field than the activation threshold
    _odc :: MovingAverage
  }
  deriving ()

makeLenses ''Column

-- | A region is a set of columns, The HTM algorithm needs to look at previously active cells to predict the next active cells.
--  Therefore a region is represented with to versions of each columns, the current and previous time step of the region.
data Region = Region
  { -- | The columns in the current time step, used by the temporal algorithm of the HTM algorithm.
    _currentStep :: [Column],
    -- | The columns in the previous time step, used by the temporal algorithm of the HTM algorthm.
    _previousStep :: [Column]
  }

makeLenses ''Region

getCell :: CellID -> [Column] -> Cell
getCell id cols = ((cols !! colId) ^. cells) !! cellId
  where
    cellId = naturalToInt (id ^. cell)
    colId = naturalToInt (id ^. col)

instance Eq Column where
  c1 == c2 = c1 ^. inputField == c2 ^. inputField

-- -------------------------------------------------------------
--                           VIEW
-- -------------------------------------------------------------

instance Show Region where
  show r = "C: " ++ show (r ^. currentStep) ++ "\nP: " ++ show (r ^. previousStep)

instance Show Column where
  -- show = show . _overlap
  --show column = intercalate "" $ map show $ _cells column
  -- show = show . _columnState
  -- show = show . _inputField
  --show = show . _adc
  show = intercalate "" . map show . _cells

-- show = show . _boost

instance Show Cell where
  show = show . _cellState

-- show cell = (show (fromEnum $ _isWinner cell)) ++  (show (_dendrites cell))
--show = show . fromEnum . _isWinner
--show = show . _dendrites

instance Show Segment where
  --show = show . _segmentState
  show = show . _synapses

instance Show Synapse where
  show = show . _destination

instance Show CellID where
  show = show . _col

instance Show SegmentState where
  show ActiveSegment = "1"
  show InActiveSegment = "0"
  show MatchingSegment = "m"

instance Show ColumnState where
  show ActiveColumn = "1"
  show InActiveColumn = "0"

instance Show CellState where
  show ActiveCell = "1"
  show InActiveCell = "0"
  show PredictiveCell = "p"
  show ActivePredictiveCell = "a"

instance Show FeedForwardSynapse where
  show = show . _conStr

-- show = show . _ind

-- -------------------------------------------------------------
--                           INITILIZE
-- -------------------------------------------------------------

-- | Initilize a region
initRegion :: SDRConfig -> RegionConfig -> IO Region
initRegion conS conR = do
  region <- initAllDendrites conR $ initColumns conS conR
  let regions = replicate 2 region -- make a copy of region
  return
    Region
      { _currentStep = head regions,
        _previousStep = head . tail $ regions
      }

-- | Initilize all columns in a region
initColumns :: SDRConfig -> RegionConfig -> IO [Column]
initColumns conS conR = mapM (initsingleColumn conS conR) [0 .. conR ^. nrOfColumns]

-- | Initilize a column
initsingleColumn :: SDRConfig -> RegionConfig -> Natural -> IO Column
initsingleColumn conS conR columnIndex = do
  fs <- initFeedForwardSynapses conS conR
  let c =
        Column
          { _columnId = columnIndex,
            _cells = initCells conR columnIndex,
            _inputField = fs,
            _odc = MovingAverage {_bits = [], _window = conR ^. mvWindow}, -- TODO the average rate of activation
            _adc = MovingAverage {_bits = [], _window = conR ^. mvWindow},
            _columnState = InActiveColumn,
            _boost = 1, -- should maybe be Float
            _overlap = 0,
            _inhibRad = 2 -- how to select!
          }
  return c

-- | Initilize all cells in a columns
initCells :: RegionConfig -> Natural -> [Cell]
initCells conR colIndex = [singleCell colIndex cellIndex | cellIndex <- [0 .. (conR ^. nrOfCellsPerColumn)]]

-- | Initilize a cell
singleCell :: Natural -> Natural -> Cell
singleCell colIndex cellIndex =
  Cell
    { _cellId = CellID {_col = colIndex, _cell = cellIndex},
      _dendrites = [], -- The dendrites are initilized after all cells are initilized.
      _cellState = InActiveCell,
      _isWinner = False
    }

-- | Initilize a list of Dendrite for all columns
initAllDendrites :: RegionConfig -> IO [Column] -> IO [Column]
initAllDendrites conR columns = do
  cols <- columns
  mapM (initDendritesPerColumn conR cols) cols

-- | Initilize a list of Dendrite for a column
initDendritesPerColumn :: RegionConfig -> [Column] -> Column -> IO Column
initDendritesPerColumn conR columns column = do
  cells <- mapM (initDendritesPerCell conR columns) (column ^. cells)
  return
    column
      { _cells = cells
      }

-- | Initilize a list of dendrites for a each cell
initDendritesPerCell :: RegionConfig -> [Column] -> Cell -> IO Cell
initDendritesPerCell conR columns cell = do
  initDend <- initDendrites conR cell columns
  return
    cell
      { _dendrites = initDend
      }

-- | Initilize a list of dendrites
initDendrites :: RegionConfig -> Cell -> [Column] -> IO [Dendrite]
initDendrites conR cell columns = do
  syns <- mapM (initSynapse conR cell columns) [1 .. (conR ^. nrOfSynapsesPerSegment)]
  let segm =
        Segment
          { _segmentState = InActiveSegment,
            _synapses = syns,
            _matchingStrength = 0
          }
  return [[segm]] -- _nrOfSynapsesPerSegment synapses in one segment in one dendrite

initSynapse :: RegionConfig -> Cell -> [Column] -> Natural -> IO Synapse -- TODO create synapse connection to the _previousStep from the CurrentStep
initSynapse conR cell columns _ = do
  destCell <- getRandomCell conR cell columns
  let syn =
        Synapse
          { _source = cell ^. cellId,
            _destination = destCell ^. cellId,
            _connectionStrength = conR ^. initConnectionStrength
          }
  return syn

-- TODO fix this, it is ugly!
getRandomCell :: RegionConfig -> Cell -> [Column] -> IO Cell
getRandomCell conR notCell columns = do
  randColumnIndex <- naturalToInt <$> getRandomIndexBetween 1 (conR ^. nrOfColumns)
  randCellIndex <- naturalToInt <$> getRandomIndexBetween 1 (conR ^. nrOfCellsPerColumn)
  let randCell = ((columns !! randColumnIndex) ^. cells) !! randCellIndex
  if randCell == notCell -- the cell is always the same at the beginning. Needs indexing
    then getRandomCell conR notCell columns
    else return randCell

-- | init mapping between sdr indecies and Columns in the region
initFeedForwardSynapses :: SDRConfig -> RegionConfig -> IO [FeedForwardSynapse]
initFeedForwardSynapses conS conR = mapM (singleFeedForwardSynapse conR) $ selectRandomIndecies conS conR -- FIXME this is a list of the synapses

selectRandomIndecies :: SDRConfig -> RegionConfig -> [IO BitIndex]
selectRandomIndecies conS conR = randIndecies (conR ^. maxNrOfInputBits) (conS ^. sdrRange)

randIndecies :: Natural -> SDRRange -> [IO BitIndex]
randIndecies n sR
  | n <= 0 = []
  | n > 0 = randIndex sR : randIndecies (n -1) sR -- TODO double check that no duplicates occure

randIndex :: SDRRange -> IO BitIndex
randIndex cR = getRandomIndexBetween (cR ^. minIndex) (cR ^. maxIndex)

getRandomIndexBetween :: Natural -> Natural -> IO BitIndex
getRandomIndexBetween mi ma = do
  let mii = naturalToInt mi
  let mai = naturalToInt ma
  intToNatural <$> getStdRandom (randomR (mii, mai)) -- returns an IO BitIndex

singleFeedForwardSynapse :: RegionConfig -> IO BitIndex -> IO FeedForwardSynapse
singleFeedForwardSynapse config index = do
  indexVal <- index
  let f =
        FeedForwardSynapse
          { _ind = indexVal,
            _conStr = config ^. initConnectionStrength
          }
  return f

-- TODO set initConnectionStrength defined by a kernel function.

-- TODO double check that no duplicates occure
-- FIXME this is a list of the synapses
-- todo need a random seed
-- TODO set initConnectionStrength defined by a kernel function.
