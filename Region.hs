{-# LANGUAGE TemplateHaskell #-}
module Region where

import           MovingAverage 
import           System.Random
import           Data.List     (intercalate)
import SDR(BitIndex(..), SDRRange(..), SDRConfig(..))
import Control.Lens hiding (element)


-- -------------------------------------------------------------
--                           CONFIG
-- -------------------------------------------------------------

-- |The configuration parameters for a Region.
data RegionConfig = RegionConfig{
  _nrOfColumns              :: Int -- ^ The number of columns in the region.
  , _nrOfCellsPerColumn     :: Int -- ^ The number of cells per column in the region. It is the same for all cells.
  , _maxNrOfInputBits       :: Int -- ^ The maximum number of input bits in the inputfield connected to a Column.
  , _nrOfSynapsesPerSegment :: Int -- ^ The number of initial synapses per segment.
  , _mappingType            :: MappingType -- ^ The mapping type between an input sdr and region columns -- TODO delete
  , _initConnectionStrength :: Float -- ^ The connection strength of synapses between cells in a region.
  , _mvWindow :: Int -- ^The size of the window of the moving average, used in overlap and active duty cycle.
}

-- -------------------------------------------------------------
--                           MODEL
-- -------------------------------------------------------------
-- |Mapper between SDR and Region
data MappingType = Random

-- |A region is a set of columns, The HTM algorithm needs to look at previously active cells to predict the next active cells. 
-- Therefore a region is represented with to versions of each columns, the current and previous time step of the region.
data Region = Region {
  _currentStep  :: [Column], -- ^The columns in the current time step, use by the temporal algorithm of the HTM algorithm.
  _previousStep :: [Column] -- ^The columns in the previous time step, used by the temporal algorithm of the HTM algorthm.
}


data Column = Column {
  _columnId :: ColumnIndex -- The index of the column. 
  , _cells       :: [Cell] -- ^The cells in a column. When a column is active, one or more of these cell will represent that state. These cells get the same input, i.e. the input to the column they belong to.
  , _inputField  :: [FeedForwardSynapse] -- ^The bit indecies this columns is connected to in the SDR.
  , _columnState :: ColumnState -- ^The state of this column. 
  , _boost       :: Float -- ^TODO should maybe be Float, should be at least 1. If a column has small moving activation average, it is boosted within the inhibition algorithm. This enforces sparcity.
  , _overlap     :: Int -- ^The number of active SDRbits this column is connected to. This value can be boosted by the boost factor.
  , _inhibRad    :: Int -- ^The radius of inhibition i.e. the inhibition algorithm looks at the neighbors of this column, this value determines range of the neighbor.
  , _adc :: MovingAverage -- ^Active duty cylce. The moving Average rate of how often this column is activated.
  , _odc :: MovingAverage -- ^Overlap duty cycle. The moving average rate of how often this column had bigger overlap with input field than the activation threshold
}

-- |A tuple containing a bit index in the inputSDR along with a connecition strength
data FeedForwardSynapse = FeedForwardSynapse{
    _ind :: BitIndex, 
    _conStr :: ConnectionStrength
} deriving(Eq)

-- |A cell has input dendrites and a cell state.
data Cell = Cell {
  _cellId :: CellID -- ^A unique Id representing the cell within a region 
  , _dendrites :: [Dendrite] -- ^A set of dendrites. 
  , _cellState :: CellState -- ^The state of this cell.
  , _isWinner :: Bool
} deriving (Eq)

data CellID = CellID{
  _col :: ColumnIndex -- ^The column 
  , _cell :: CellIndex
} deriving(Eq)

-- |The two states a column can exist in.
data ColumnState = ActiveColumn | InActiveColumn deriving (Eq)
-- |The three states a cell can exist in.
data CellState = ActiveCell | InActiveCell | PredictiveCell | ActivePredictiveCell deriving (Eq)

data SegmentState = ActiveSegment | InActiveSegment deriving(Eq)
-- |A collection of segments.
type Dendrite = [Segment] -- These dendrites can come from other regions

-- |A collection of synapses.
data Segment = Segment{
  _segmentState :: SegmentState -- ^The state of this segment. 
  , _synapses :: [Synapse] -- ^These segmenst together define a dendrite. Each segment has is expected to encode atleast one pattern to recognise.
  , _matchingStrength :: Int -- ^The number of active synpases.
} deriving(Eq)
-- |A synapse is the connection between two cells. A synapse is part of a group of synapses called segment. 
-- Each segment is attached to a dendrite.
data Synapse = Synapse {
  _source               :: Cell -- ^where the input is coming from
  , _destination        :: CellID -- ^where the input is going too.
  , _connectionStrength :: ConnectionStrength -- ^i.e. the connection strength between the source and destination
} deriving (Eq)

type ConnectionStrength = Float -- ^Between 0 and 1
type ColumnIndex = Int -- ^unsigned int
type CellIndex = Int -- ^unsigned int
type DendriteIndex = Int -- ^unsigned int
type SegmentIndex = Int -- ^unsigned int
type Index = Int

------ Lenses

makeLenses ''Region
makeLenses ''Column
makeLenses ''Cell
makeLenses ''Segment
makeLenses ''Synapse
makeLenses ''FeedForwardSynapse
makeLenses ''RegionConfig
makeLenses ''CellID

getCell :: CellID -> [Column] -> Cell
getCell id cols = ((cols !! (id^.col))^.cells) !! (id^.cell)

-- -------------------------------------------------------------
--                           VIEW
-- -------------------------------------------------------------

instance Show Region where
  show = show . _currentStep 

instance Eq Column where
  c1 == c2 =  c1^.inputField ==  c2^.inputField

instance Show Column where
  show = show . _overlap 
  --show column = intercalate "" $ map show $ _cells column
  --show = show . _columnState
  --show = show . _inputField
  --show = show . _adc 
  --show = show . _cells

instance Show Cell where
  show = show . _cellState 

instance Show ColumnState where
  show ActiveColumn = "1"
  show InActiveColumn = "0"

instance Show CellState where
  show ActiveCell = "1"
  show InActiveCell = "0"
  show PredictiveCell = "p"

instance Show FeedForwardSynapse where
  show = show . _conStr


-- -------------------------------------------------------------
--                           INITILIZE
-- -------------------------------------------------------------

-- | Initilize a region
initRegion :: SDRConfig -> RegionConfig -> IO Region
initRegion conS conR = do
  region <- initAllDendrites conR $ initColumns conS conR
  let regions = replicate 2 region -- make a copy of region
  return Region {
    _currentStep = head regions
    , _previousStep = head . tail $ regions
    }


-- | Initilize all columns in a region
initColumns :: SDRConfig -> RegionConfig -> IO [Column]
initColumns conS conR = mapM (initsingleColumn conS conR) [0.. conR^.nrOfColumns]

-- | Initilize a column
initsingleColumn :: SDRConfig -> RegionConfig -> Int -> IO Column
initsingleColumn  conS conR columnIndex = do 
    fs <- initFeedForwardSynapses conS conR
    let c = Column {
      _columnId = columnIndex
    , _cells = initCells conR columnIndex
    , _inputField = fs
    , _odc = MovingAverage {_bits = [], _window = conR^.mvWindow} -- TODO the average rate of activation
    , _adc = MovingAverage {_bits = [], _window = conR^.mvWindow}
    , _columnState = InActiveColumn
    , _boost       = 1 -- should maybe be Float
    , _overlap     = 0
    , _inhibRad    = 2 -- how to select!
    }
    return c

-- | Initilize all cells in a columns
initCells :: RegionConfig -> Int -> [Cell]
initCells conR colIndex = [singleCell colIndex cellIndex | cellIndex <- [0..( conR^.nrOfCellsPerColumn)]]

-- | Initilize a cell
singleCell :: Int -> Int -> Cell
singleCell colIndex cellIndex = Cell {
  _cellId = CellID{_col = colIndex, _cell = cellIndex}
  , _dendrites = [] -- The dendrites are initilized after all cells are initilized.
  , _cellState = InActiveCell
  , _isWinner = False
}


-- | Initilize a list of Dendrite for all columns
initAllDendrites :: RegionConfig -> IO [Column] -> IO [Column]
initAllDendrites conR columns = do 
  cols <- columns
  mapM (initDendritesPerColumn conR cols) cols

-- |Initilize a list of Dendrite for a column
initDendritesPerColumn ::  RegionConfig -> [Column] -> Column -> IO Column
initDendritesPerColumn conR columns column = do
  cells <- mapM (initDendritesPerCell conR columns) (column^.cells)
  return column {
  _cells = cells 
  }

-- |Initilize a list of dendrites for a each cell
initDendritesPerCell :: RegionConfig -> [Column] -> Cell -> IO Cell
initDendritesPerCell conR columns cell = do
  initDend <- initDendrites conR cell columns
  return cell {
  _dendrites = initDend
}



-- |Initilize a list of dendrites
initDendrites :: RegionConfig -> Cell -> [Column] -> IO [Dendrite]
initDendrites conR cell columns = do 

  syns <- mapM (initSynapse conR cell columns) [1..(conR^.nrOfSynapsesPerSegment)]
  let segm = Segment{
    _segmentState = InActiveSegment
    , _synapses = syns
    , _matchingStrength = 0
  }
  return [[segm]]-- _nrOfSynapsesPerSegment synapses in one segment in one dendrite


initSynapse :: RegionConfig -> Cell -> [Column] -> Int -> IO Synapse -- TODO create synapse connection to the _previousStep from the CurrentStep
initSynapse conR cell columns _ = do
  destCell <- getRandomCell conR cell columns
  let syn = Synapse {
  _source = cell
  , _destination = destCell^.cellId
  , _connectionStrength = conR^.initConnectionStrength
  }
  return syn

-- TODO fix this, it is ugly!
getRandomCell :: RegionConfig -> Cell -> [Column] -> IO Cell
getRandomCell conR notCell columns = do 
  randColumnIndex <- getRandomIndexBetween 1 (conR^.nrOfColumns)
  randCellIndex <- getRandomIndexBetween 1 (conR^.nrOfCellsPerColumn )
  let randCell = ((columns !! randColumnIndex)^.cells) !! randCellIndex
  if randCell == notCell -- the cell is always the same at the beginning. Needs indexing
    then getRandomCell conR notCell columns 
    else return randCell


-- |init mapping between sdr indecies and Columns in the region
initFeedForwardSynapses :: SDRConfig -> RegionConfig ->  IO [FeedForwardSynapse]
initFeedForwardSynapses conS conR = mapM (singleFeedForwardSynapse conR) $ selectRandomIndecies conS conR -- FIXME this is a list of the synapses 

selectRandomIndecies :: SDRConfig -> RegionConfig ->  [IO BitIndex]
selectRandomIndecies  conS conR = randIndecies ( conR^.maxNrOfInputBits) (sdrRange conS)

randIndecies :: Int -> SDRRange -> [IO BitIndex]
randIndecies n sR
  | n <= 0 = []
  | n > 0 = randIndex sR : randIndecies (n-1) sR -- TODO double check that no duplicates occure


randIndex :: SDRRange -> IO BitIndex
randIndex cR =  getRandomIndexBetween (minIndex cR) (maxIndex cR)

getRandomIndexBetween :: Int -> Int -> IO BitIndex
getRandomIndexBetween mi ma = do
  getStdRandom $ randomR (mi,ma) -- returns an IO BitIndex

singleFeedForwardSynapse :: RegionConfig -> IO BitIndex -> IO FeedForwardSynapse
singleFeedForwardSynapse config index  = do 
  indexVal <- index
  let f = FeedForwardSynapse{
    _ind = indexVal, 
    _conStr = config^.initConnectionStrength 
    } 
  return f
     -- TODO set initConnectionStrength defined by a kernel function.


-- TODO double check that no duplicates occure
-- FIXME this is a list of the synapses 
-- todo need a random seed
-- TODO set initConnectionStrength defined by a kernel function.
