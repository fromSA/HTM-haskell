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
  dendrites   :: [Dendrite] -- ^A set of dendrites. 
  , cellState :: CellState -- ^The state of this cell.
} deriving (Eq)

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
  c1 == c2 = (inputField c1) == (inputField c2)

instance Show Column where
  --show  = show . overlap 
  show column = intercalate "" $ map show $ cells column

instance Show Cell where
  show = show . cellState 

instance Show ColumnState where
  show ActiveColumn = "1"
  show InactiveColumn = "0"

instance Show CellState where
  show ActiveCell = "1"
  show InactiveCell = "0"
  show PredictiveCell = "p"


-- -------------------------------------------------------------
--                           INITILIZE
-- -------------------------------------------------------------

-- | Initilize a region
initRegion :: SDRConfig -> RegionConfig -> Region
initRegion conS conR =
  let regions = replicate 2 $ initAllDendrites conR $ initColumns conS conR in -- make a copy of region
    Region {
    currentStep = head regions
    , previousStep = head . tail $ regions
    }

-- | Initilize all columns in a region
initColumns :: SDRConfig -> RegionConfig -> [Column]
initColumns conS conR = map (initsingleColumn conS conR) $ [0..nrOfColumns conR]

-- | Initilize a column
initsingleColumn :: SDRConfig -> RegionConfig -> Int -> Column
initsingleColumn  conS conR columnIndex = Column {
  cells = initCells conR
  , inputField = initFeedForwardSynapses conS conR columnIndex
  , odc = MovingAverage {_bits = [], _window = (mvWindow conR)} -- TODO the average rate of activation
  , adc = MovingAverage {_bits = [], _window = (mvWindow conR)}
  , columnState = InactiveColumn
  , boost       = 1 -- should maybe be Float
  , overlap     = 0
  , inhibRad    = 0
}

-- | Initilize all cells in a columns
initCells :: RegionConfig -> [Cell]
initCells conR = [singleCell | _ <- [0..(nrOfCellsPerColumn conR)]]

-- | Initilize a cell
singleCell :: Cell
singleCell = Cell {
  dendrites = [] -- The dendrites are initilized after all cells are initilized.
  , cellState = InactiveCell
}


-- | Initilize a list of Dendrite for all columns
initAllDendrites :: RegionConfig -> [Column] -> [Column]
initAllDendrites conR columns = map (initDendritesPerColumn conR columns) columns

-- |Initilize a list of Dendrite for a column
initDendritesPerColumn ::  RegionConfig -> [Column] -> Column -> Column
initDendritesPerColumn conR columns column = column {
  cells = map (initDendritesPerCell conR columns) (cells column)
}

-- |Initilize a list of dendrites for a each cell
initDendritesPerCell :: RegionConfig -> [Column] -> Cell -> Cell
initDendritesPerCell conR columns cell = cell {
  dendrites = initDendrites conR cell columns
}



-- |Initilize a list of dendrites
initDendrites :: RegionConfig -> Cell -> [Column] -> [Dendrite]
initDendrites conR cell columns = [[[initSynapse conR cell columns | _ <- [1.. (nrOfSynapsesPerSegment conR)] ]]]--[initDendrite conS conR cell columns]

initSynapse :: RegionConfig -> Cell -> [Column] -> Synapse
initSynapse conR cell columns = Synapse {
  source = cell
  , destination = getRandomCell conR cell columns
  , connectionStrength = initConnectionStrength conR
}

-- TODO fix this, it is ugly!
getRandomCell :: RegionConfig -> Cell -> [Column] -> Cell
getRandomCell conR notCell columns = if randCell == notCell 
    then getRandomCell conR notCell columns 
    else randCell
    where 
    randColumnIndex = getRandomIndexBetween 0 1 (nrOfColumns conR) -- todo need a random seed
    randCellIndex = getRandomIndexBetween 0 1 (nrOfCellsPerColumn conR)-- todo need a random seed
    randCell = (cells (columns !! randColumnIndex)) !! randCellIndex



-- |init mapping between sdr indecies and Columns in the region
initFeedForwardSynapses :: SDRConfig -> RegionConfig -> ColumnIndex ->  [FeedForwardSynapse]
initFeedForwardSynapses conS conR c = map (singleFeedForwardSynapse conR) $ selectRandomIndecies conS conR c -- FIXME this is a list of the synapses 

selectRandomIndecies :: SDRConfig -> RegionConfig -> ColumnIndex ->  [BitIndex]
selectRandomIndecies  conS conR cI = randIndecies (maxNrOfInputBits conR) cI (sdrRange conS)

randIndecies :: Int -> ColumnIndex -> SDRRange -> [BitIndex]
randIndecies n cI sR
  | n <= 0 = []
  | n > 0 = randIndex cI sR : randIndecies (n-1) cI sR -- TODO double check that no duplicates occure

getRandomIndexBetween :: Int -> Int -> Int -> BitIndex
getRandomIndexBetween seed mi ma = let g = mkStdGen seed in
                                      fst (randomR (mi, ma) g)

randIndex :: ColumnIndex -> SDRRange -> BitIndex
randIndex cI cR =  getRandomIndexBetween cI (minIndex cR) (maxIndex cR)

singleFeedForwardSynapse :: RegionConfig -> BitIndex -> FeedForwardSynapse
singleFeedForwardSynapse config index  = FeedForwardSynapse{
    ind = index, 
    conStr = (initConnectionStrength config)
    } -- TODO set initConnectionStrength defined by a kernel function.


-- TODO double check that no duplicates occure
-- FIXME this is a list of the synapses 
-- todo need a random seed
-- TODO set initConnectionStrength defined by a kernel function.