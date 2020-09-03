module HTM (spacialPooler, temporalPooler) where

import           System.Random

-- MODEL

-- Config
data Config = Config {
  nrOfColumns              :: Int
  , nrOfCellsPerColumn     :: Int
  , maxNrOfInputBits       :: Int
  , nrOfSynapsesPerCell    :: Int
  , mappingType            :: MappingType
  , initConnectionStrength :: Float -- on the synapses between cells in a region
  , sdrRange               :: SDRRange
}


-- SDR
data Encoder = Numeric | Categorical
{- Numeric Range
  | NumbericLog -- These are types of encodings.
  | Delta
  | Category Cyclic Order
  | Geospatial Range Speed
  | Text

data Range = Bounded | UnBounded
data InputValue = Number | Vector
data Number = Continues Range | Discrete Range
-}
data SDRRange = InputField{
  minIndex  :: BitIndex
  ,maxIndex :: BitIndex
  }
type SDR = [BitIndex]
type BitIndex = Int


-- Mapper between SDR and Region
data MappingType = Random



-- Region
data Region = Region {
  currentStep  :: [Column],
  previousStep :: [Column]
}

data Column = Column {
  cells         :: [Cell]
  , inputField  :: [FeedForwardSynapse]
  , howActive   :: Float -- TODO the average rate of activation
  , columnState :: ColumnState
}
type FeedForwardSynapse = (BitIndex, ConnectionStrength)

data Cell = Cell {
  dendrites   :: [Dendrite]
  , cellState :: CellState
} deriving (Eq)

data ColumnState = ActiveColumn | InactiveColumn deriving (Eq)
data CellState = ActiveCell | InactiveCell | PredictiveCell deriving (Eq)

type Dendrite = [Segment]
type Segment = [Synapse]
data Synapse = Synapse {
  source               :: Cell -- where the input is coming from
  , destination        :: Cell -- where the input is going too
  , connectionStrength :: ConnectionStrength -- i.e. permenanceValue
} deriving (Eq)
type ConnectionStrength = Float -- Between 0 and 1
type ColumnIndex = Int -- unsigned int
type CellIndex = Int -- unsigned int

-- for each Column[i] -> create a choose random of indexes from the SDR
-- Initialize

initRegion :: Config -> Region  -- TODO do I initialze the segments?
initRegion config =
  let columns = initDendrites config $ initColumns config in
    Region {
    currentStep = columns
    , previousStep = (replicate 2 columns) !! 1 -- make a copy of region by using (replicate 2 region)
    }

initColumns :: Config -> [Column]
initColumns config = [singleColumn columnIndex config | columnIndex <- [0..nrOfColumns config]]

singleColumn :: Int -> Config -> Column
singleColumn columnIndex config = Column {
  cells = initCells config
  , inputField = initFeedForwardSynapses columnIndex config
  , howActive = 0.0 -- TODO the average rate of activation
  , columnState = InactiveColumn
}

initCells :: Config -> [Cell]
initCells config = [singleCell | _ <- [0..(nrOfCellsPerColumn config)]]

singleCell :: Cell
singleCell = Cell {
  dendrites = []
  , cellState = InactiveCell
}




-- Init Dendrites


initDendrites :: Config -> [Column] -> [Column]
initDendrites config columns = map (initDendritePerColumn config columns) columns

initDendritePerColumn :: Config -> [Column] -> Column -> Column
initDendritePerColumn config columns column = column {
  cells = map (initDendritePerCell config columns) (cells column)
}

initDendritePerCell :: Config -> [Column] -> Cell -> Cell
initDendritePerCell config columns cell = cell {
  dendrites = createDendrites config cell columns
}


createDendrites :: Config -> Cell -> [Column] -> [Dendrite]
createDendrites config cell columns = [createDendrite config cell columns]

createDendrite :: Config -> Cell -> [Column] -> [Segment]
createDendrite config cell columns = [createSegment config cell columns]

createSegment :: Config -> Cell -> [Column] -> [Synapse]
createSegment config cell columns = [createSynapse config cell columns | _ <- [1.. (nrOfSynapsesPerCell config)] ]

createSynapse :: Config -> Cell -> [Column] -> Synapse
createSynapse config cell columns = Synapse {
  source = cell
  , destination = getRandomCell config cell columns
  , connectionStrength = initConnectionStrength config
}


-- TODO fix this, it is ugly!
getRandomCell :: Config -> Cell -> [Column] -> Cell
getRandomCell config notCell columns = let randColumnIndex = getRandomIndexBetween 0 1 (nrOfColumns config) in -- todo need a random seed
                                          let randCellIndex = getRandomIndexBetween 0 1 (nrOfCellsPerColumn config) in -- todo need a random seed
                                            let randCell = (cells (columns !! randColumnIndex)) !! randCellIndex in
                                              if randCell == notCell
                                                then getRandomCell config notCell columns
                                                else randCell


-- init mapping between sdr indecies and Columns in the region
initFeedForwardSynapses :: ColumnIndex -> Config -> [FeedForwardSynapse]
initFeedForwardSynapses cI con = [singleFeedForwardSynapse bI con | bI <- selectRandomIndecies cI con]

selectRandomIndecies :: ColumnIndex -> Config -> [BitIndex]
selectRandomIndecies cI con = randIndecies (maxNrOfInputBits con) cI (sdrRange con)

randIndecies :: Int -> ColumnIndex -> SDRRange -> [BitIndex]
randIndecies n cI sR
  | n <= 0 = []
  | n > 0 = randIndex cI sR : randIndecies (n-1) cI sR --TODO double check that no duplicates occure


getRandomIndexBetween :: Int -> Int -> Int -> BitIndex
getRandomIndexBetween seed mi ma = let g = mkStdGen seed in
                                      fst (randomR (mi, ma) g)

randIndex :: ColumnIndex -> SDRRange -> BitIndex
randIndex cI cR =  getRandomIndexBetween cI (minIndex cR) (maxIndex cR)

singleFeedForwardSynapse :: BitIndex -> Config -> (BitIndex, ConnectionStrength)
singleFeedForwardSynapse index config = (index, initConnectionStrength config) -- TODO set initConnectionStrength defined by a kernel function.

-- UPDATE

updatePermanence :: Region -> Region
updatePermanence r = r -- TODO

boost :: Region -> Region
boost r = r -- TODO

activateColumns :: SDR-> Region -> Region
activateColumns i r = r -- TODO

-- TODO applie a spacialPooler on a region
spacialPooler :: SDR-> Region -> Region
spacialPooler i r = updatePermanence . boost . activateColumns i $ r
-- first map input to region
-- second activate columns based on their input region
-- Boost the column (to maintain a fixed sparcity)
-- update permenance values of each Sensory


-- TODO applie a spacialPooler on a region
temporalPooler :: Region -> Region
temporalPooler m = m
-- Represent input within the previous context
  -- For each active column -> activate predictive cells | activate all cells
-- Predict next state
  -- For cells with n nr of Active Dentrite -> Predict state
  -- Update PermenanceValue between

-- VIEW
