module Region where

import           MovingAverage 
import           System.Random
import           Data.List     (intercalate)
import SDR(BitIndex(..), SDRRange(..), SDRConfig(..))

-- -------------------------------------------------------------
--                           CONFIG
-- -------------------------------------------------------------

-- |The configuration parameters for a Region.
data RegionConfig = RegionConfig{
  nrOfColumns              :: Int -- ^ The number of columns in the region.
  , nrOfCellsPerColumn     :: Int -- ^ The number of cells per column in the region. It is the same for all cells.
  , maxNrOfInputBits       :: Int -- ^ The maximum number of input bits in the inputfield connected to a Column.
  , nrOfSynapsesPerSegment :: Int -- ^ The number of initial synapses per segment.
  , mappingType            :: MappingType -- ^ The mapping type between an input sdr and region columns
  , initConnectionStrength :: Float -- ^ The connection strength of synapses between cells in a region.
  , mvWindow :: Int -- ^The size of the window of the moving average, used in overlap and active duty cycle.
}


-- -------------------------------------------------------------
--                           MODEL
-- -------------------------------------------------------------

-- |Mapper between SDR and Region
data MappingType = Random

-- |A region is a set of columns, The HTM algorithm needs to look at previously active cells to predict the next active cells. 
-- Therefore a region is represented with to versions of each columns, the current and previous time step of the region.
data Region = Region {
  currentStep  :: [Column], -- ^The columns in the current time step, use by the temporal algorithm of the HTM algorithm.
  previousStep :: [Column] -- ^The columns in the previous time step, used by the temporal algorithm of the HTM algorthm.
}


data Column = Column {
  cells         :: [Cell] -- ^The cells in a column. When a column is active, one or more of these cell will represent that state. These cells get the same input, i.e. the input to the column they belong to.
  , inputField  :: [FeedForwardSynapse] -- ^The bit indecies this columns is connected to in the SDR.
  , columnState :: ColumnState -- ^The state of this column. 
  , boost       :: Float -- ^TODO should maybe be Float, should be at least 1. If a column has small moving activation average, it is boosted within the inhibition algorithm. This enforces sparcity.
  , overlap     :: Int -- ^The number of active SDRbits this column is connected to. This value can be boosted by the boost factor.
  , inhibRad    :: Int -- ^The radius of inhibition i.e. the inhibition algorithm looks at the neighbors of this column, this value determines range of the neighbor.
  , adc :: MovingAverage -- ^Active duty cylce. The moving Average rate of how often this column is activated.
  , odc :: MovingAverage -- ^Overlap duty cycle. The moving average rate of how often this column had bigger overlap with input field than the activation threshold
}

-- |A tuple containing a bit index in the inputSDR along with a connecition strength
data FeedForwardSynapse = FeedForwardSynapse{
    ind :: BitIndex, 
    conStr :: ConnectionStrength
} deriving(Eq)

-- |A cell has input dendrites and a cell state.
data Cell = Cell {
  id_ :: ID -- ^A unique Id representing the cell within a region 
  , dendrites :: [Dendrite] -- ^A set of dendrites. 
  , cellState :: CellState -- ^The state of this cell.
} deriving (Eq)

data ID = ID{
  col_ :: ColumnIndex -- ^The column 
  , cell_ :: CellIndex
} deriving(Eq)

-- |The two states a column can exist in.
data ColumnState = ActiveColumn | InactiveColumn deriving (Eq)
-- |The three states a cell can exist in.
data CellState = ActiveCell | InactiveCell | PredictiveCell deriving (Eq)

-- |A collection of segments.
type Dendrite = [Segment]
-- |A collection of synapses.
type Segment = [Synapse]

-- |A synapse is the connection between two cells. A synapse is part of a group of synapses called segment. 
-- Each segment is attached to a dendrite.
data Synapse = Synapse {
  source               :: Cell -- ^where the input is coming from
  , destination        :: Cell -- ^where the input is going too
  , connectionStrength :: ConnectionStrength -- ^i.e. the connection strength between the source and destination
} deriving (Eq)

type ConnectionStrength = Float -- ^Between 0 and 1
type ColumnIndex = Int -- ^unsigned int
type CellIndex = Int -- ^unsigned int

-- -------------------------------------------------------------
--                           VIEW
-- -------------------------------------------------------------

instance Show Region where
  show = show . currentStep 

instance Eq Column where
  c1 == c2 = inputField c1 == inputField c2

instance Show Column where
  --show = show . overlap 
  --show column = intercalate "" $ map show $ cells column
  show = show . columnState
  --show = show . inputField
  --show = show . adc 

instance Show Cell where
  show = show . cellState 

instance Show ColumnState where
  show ActiveColumn = "1"
  show InactiveColumn = "0"

instance Show CellState where
  show ActiveCell = "1"
  show InactiveCell = "0"
  show PredictiveCell = "p"

instance Show FeedForwardSynapse where
  show = show . conStr


-- -------------------------------------------------------------
--                           INITILIZE
-- -------------------------------------------------------------

-- | Initilize a region
initRegion :: SDRConfig -> RegionConfig -> IO Region
initRegion conS conR = do
  region <- initAllDendrites conR $ initColumns conS conR
  let regions = replicate 2 region -- make a copy of region
  return Region {
    currentStep = head regions
    , previousStep = head . tail $ regions
    }


-- | Initilize all columns in a region
initColumns :: SDRConfig -> RegionConfig -> IO [Column]
initColumns conS conR = mapM (initsingleColumn conS conR) [0..nrOfColumns conR]

-- | Initilize a column
initsingleColumn :: SDRConfig -> RegionConfig -> Int -> IO Column
initsingleColumn  conS conR columnIndex = do 
    fs <- initFeedForwardSynapses conS conR
    let c = Column {
    cells = initCells conR columnIndex
    , inputField = fs
    , odc = MovingAverage {_bits = [], _window = mvWindow conR} -- TODO the average rate of activation
    , adc = MovingAverage {_bits = [], _window = mvWindow conR}
    , columnState = InactiveColumn
    , boost       = 1 -- should maybe be Float
    , overlap     = 0
    , inhibRad    = 2 -- how to select!
    }
    return c

-- | Initilize all cells in a columns
initCells :: RegionConfig -> Int -> [Cell]
initCells conR colIndex = [singleCell colIndex cellIndex | cellIndex <- [0..(nrOfCellsPerColumn conR)]]

-- | Initilize a cell
singleCell :: Int -> Int -> Cell
singleCell colIndex cellIndex = Cell {
  id_ = ID{col_ = colIndex, cell_ = cellIndex}
  , dendrites = [] -- The dendrites are initilized after all cells are initilized.
  , cellState = InactiveCell
}


-- | Initilize a list of Dendrite for all columns
initAllDendrites :: RegionConfig -> IO [Column] -> IO [Column]
initAllDendrites conR columns = do 
  cols <- columns
  mapM (initDendritesPerColumn conR cols) cols

-- |Initilize a list of Dendrite for a column
initDendritesPerColumn ::  RegionConfig -> [Column] -> Column -> IO Column
initDendritesPerColumn conR columns column = do
  cells <- mapM (initDendritesPerCell conR columns) (cells column)
  return column {
  cells = cells 
  }

-- |Initilize a list of dendrites for a each cell
initDendritesPerCell :: RegionConfig -> [Column] -> Cell -> IO Cell
initDendritesPerCell conR columns cell = do
  initDend <- initDendrites conR cell columns
  return cell {
  dendrites = initDend
}



-- |Initilize a list of dendrites
initDendrites :: RegionConfig -> Cell -> [Column] -> IO [Dendrite]
initDendrites conR cell columns = do 
  segm <- mapM (initSynapse conR cell columns) [1..(nrOfSynapsesPerSegment conR)]
  return [[segm]]-- nrOfSynapsesPerSegment synapses in one segment in one dendrite


initSynapse :: RegionConfig -> Cell -> [Column] -> Int -> IO Synapse
initSynapse conR cell columns _ = do
  destCell <- getRandomCell conR cell columns
  let syn = Synapse {
  source = cell
  , destination = destCell
  , connectionStrength = initConnectionStrength conR
  }
  return syn

-- TODO fix this, it is ugly!
getRandomCell :: RegionConfig -> Cell -> [Column] -> IO Cell
getRandomCell conR notCell columns = do 
  randColumnIndex <- getRandomIndexBetween 1 (nrOfColumns conR)
  randCellIndex <- getRandomIndexBetween 1 (nrOfCellsPerColumn conR)
  let randCell = cells (columns !! randColumnIndex) !! randCellIndex
  if randCell == notCell -- the cell is always the same at the beginning. Needs indexing
    then getRandomCell conR notCell columns 
    else return randCell


-- |init mapping between sdr indecies and Columns in the region
initFeedForwardSynapses :: SDRConfig -> RegionConfig ->  IO [FeedForwardSynapse]
initFeedForwardSynapses conS conR = mapM (singleFeedForwardSynapse conR) $ selectRandomIndecies conS conR -- FIXME this is a list of the synapses 

selectRandomIndecies :: SDRConfig -> RegionConfig ->  [IO BitIndex]
selectRandomIndecies  conS conR = randIndecies (maxNrOfInputBits conR) (sdrRange conS)

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
    ind = indexVal, 
    conStr = initConnectionStrength config
    } 
  return f
     -- TODO set initConnectionStrength defined by a kernel function.


-- TODO double check that no duplicates occure
-- FIXME this is a list of the synapses 
-- todo need a random seed
-- TODO set initConnectionStrength defined by a kernel function.
