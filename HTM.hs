module HTM (spacialPooler, temporalPooler) where

import           Data.List     (intercalate, sortOn)
import           System.Random
{-

  MODEL

-}

-- Config
data Config = Config {
  nrOfColumns              :: Int
  , nrOfCellsPerColumn     :: Int
  , maxNrOfInputBits       :: Int
  , nrOfSynapsesPerCell    :: Int
  , mappingType            :: MappingType
  , initConnectionStrength :: Float -- on the synapses between cells in a region
  , sdrRange               :: SDRRange
  , overlapThreshold       :: Int -- between inputField and column
  , pConthresh             :: Float -- permanence connected threshold on a synapse
  , colActLev              :: Int -- the desired column activity level within inhibition radius
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
data SDRRange = SDRRange{
  minIndex  :: BitIndex -- TODO remove this for an implisit minIndex = 0
  ,maxIndex :: BitIndex
  }
type SDR = [BitIndex]
type BitIndex = Int

data SDRConfig = SDRConfig{
  minVal          :: Int
  , maxVal        :: Int
  , buckets       :: Int
  , bitsPerBucket :: Int
}

-- The total number of bits used SDR
totNrBits :: SDRConfig -> Int
totNrBits config = sum (map ($ config) [buckets, bitsPerBucket]) - 1

-- TODO take an input and convert it to an SDR
encode :: Int -> SDRConfig -> SDR
encode n config = let start = getStartOf n config in
  [start + i | i <- [0..(bitsPerBucket config - 1)]]

getStartOf :: Int -> SDRConfig -> Int
getStartOf n config = floor $  realToFrac ((buckets config) * (n - (minVal config))) / (realToFrac ((maxVal config) - (minVal config)))


-- Mapper between SDR and Region
data MappingType = Random


-- REGION
data Region = Region {
  currentStep  :: [Column],
  previousStep :: [Column]
}

instance Show Region where
  show = show . currentStep 

data Column = Column {
  cells         :: [Cell]
  , inputField  :: [FeedForwardSynapse]
  , howActive   :: MovingAverage -- TODO the average rate of activation
  , columnState :: ColumnState
  , boost       :: Float -- should maybe be Float, should be at least 1
  , overlap     :: Int
  , inhibRad    :: Int
}

instance Eq Column where
  c1 == c2 = (inputField c1) == (inputField c2)

instance Show Column where
  show  = show . overlap 
  --show column = intercalate "" $ map show $ cells column

type FeedForwardSynapse = (BitIndex, ConnectionStrength)

instance Show Cell where
  show = show . cellState 

data Cell = Cell {
  dendrites   :: [Dendrite]
  , cellState :: CellState
} deriving (Eq)



data ColumnState = ActiveColumn | InactiveColumn deriving (Eq)
instance Show ColumnState where
  show ActiveColumn = "1"
  show InactiveColumn = "0"

data CellState = ActiveCell | InactiveCell | PredictiveCell deriving (Eq)

instance Show CellState where
  show ActiveCell = "1"
  show InactiveCell = "0"
  show PredictiveCell = "p"

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

-- MovingAverage
data MovingAverage = MovingAverage [BitIndex] deriving (Show)

-- get the average of MovingAverage
getAverage :: MovingAverage -> Float
getAverage (MovingAverage bits) = fromIntegral (sum bits) / fromIntegral (length bits)

-- append 1 to the MovingAverage
on :: MovingAverage -> MovingAverage
on (MovingAverage bits) = (MovingAverage (1 : bits))

-- append a 0 to MovingAverage
off :: MovingAverage -> MovingAverage
off (MovingAverage bits) = (MovingAverage (0 : bits))


-- for each Column[i] -> create a choose random of indexes from the SDR

{-

  Initialize

-}

initRegion :: Config -> Region
initRegion config =
  let regions = replicate 2 $ initDendrites config $ initColumns config in -- make a copy of region
    Region {
    currentStep = head regions
    , previousStep = head . tail $ regions
    }

initColumns :: Config -> [Column]
initColumns config = [singleColumn columnIndex config | columnIndex <- [0..nrOfColumns config]]

singleColumn :: Int -> Config -> Column
singleColumn columnIndex config = Column {
  cells = initCells config
  , inputField = initFeedForwardSynapses columnIndex config
  , howActive = (MovingAverage []) -- TODO the average rate of activation
  , columnState = InactiveColumn
  , boost       = 1 -- should maybe be Float
  , overlap     = 0
  , inhibRad    = 0
}

initCells :: Config -> [Cell]
initCells config = [singleCell | _ <- [0..(nrOfCellsPerColumn config)]]

singleCell :: Cell
singleCell = Cell {
  dendrites = []
  , cellState = InactiveCell
}


-- Init DENDRITE

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
initFeedForwardSynapses cI con = [singleFeedForwardSynapse bI con | bI <- selectRandomIndecies cI con] -- FIXME this is a list of the synapses 

selectRandomIndecies :: ColumnIndex -> Config -> [BitIndex]
selectRandomIndecies cI con = randIndecies (maxNrOfInputBits con) cI (sdrRange con)

randIndecies :: Int -> ColumnIndex -> SDRRange -> [BitIndex]
randIndecies n cI sR
  | n <= 0 = []
  | n > 0 = randIndex cI sR : randIndecies (n-1) cI sR -- TODO double check that no duplicates occure

getRandomIndexBetween :: Int -> Int -> Int -> BitIndex
getRandomIndexBetween seed mi ma = let g = mkStdGen seed in
                                      fst (randomR (mi, ma) g)

randIndex :: ColumnIndex -> SDRRange -> BitIndex
randIndex cI cR =  getRandomIndexBetween cI (minIndex cR) (maxIndex cR)

singleFeedForwardSynapse :: BitIndex -> Config -> (BitIndex, ConnectionStrength)
singleFeedForwardSynapse index config = (index, initConnectionStrength config) -- TODO set initConnectionStrength defined by a kernel function.


{-

  UPDATE

-}


{-
  SpatialPooler
-}

-- TODO applie a spacialPooler on a region
spacialPooler :: Config -> SDR -> Region -> Region
--spacialPooler c i r = updatePermanence . updateBoost $ activateColumns c i r
spacialPooler c i r = activateColumns c i r

-- initialize
    -- first map input to region -- DONE
-- phase1 : overlap
    -- activate columns based on their input field
-- phase2 : Inhibition
    -- Boost the column (to maintain a fixed sparcity)
-- phase 3 : learning
    -- update permenance values of each Sensory


activateColumns :: Config -> SDR -> Region -> Region
activateColumns config sdr region = region { currentStep = updateOverlap config sdr (currentStep region)}
--activateColumns config sdr region = region { currentStep = learn . (updateInhibition config) $ updateOverlap config sdr (currentStep region)}
-- for each column -> compute overlap & boost it by the boost factor
-- for each column -> activate columns if the column is not inhibited
-- Learning: for each active column -> update permanence
-- UpdateBoosting: for each columns ->
  -- update boosting/
  -- update active duty & overlap cycle
  -- update inhibition radius


-- Overlap
updateOverlap :: Config -> SDR -> [Column] -> [Column]
updateOverlap config sdr columns = map (computeOverlap config sdr) columns

computeOverlap :: Config -> SDR -> Column -> Column
computeOverlap config sdr column = let overlap = countOverlap config sdr (inputField column) in
  if overlap >= (overlapThreshold config) then
    column{overlap = floor $ fromIntegral(overlap) * (boost column)}
  else column{overlap = 0 }


countOverlap :: Config -> SDR -> [FeedForwardSynapse] -> Int
countOverlap config sdr = sum . map (choose config sdr) 

-- |The `choose` function chooses if a column is active or not based on
-- the connection strength and activation threshold
choose :: Config -> SDR -> FeedForwardSynapse -> Int
choose config sdr (bitIndex, connectionStrength)
  | connectionStrength >= (pConthresh config) = if elem bitIndex sdr then 1 else 0
  | otherwise = 0



--Inhibition
updateInhibition :: Config -> [Column] -> [Column]
updateInhibition config columns = [maybeActivateColumn config (col,ind) columns | (col,ind) <- zip columns [1..] ]

maybeActivateColumn :: Config -> (Column,Int) -> [Column] -> Column
maybeActivateColumn config (col,ind) cols =
  (maybeActivate col) . (kmaxOverlap (colActLev config)) $ neighbors (inhibRad col) ind cols

neighbors :: Int -> Int -> [Column] -> [Column]
neighbors inhibRad colIndex cols = drop (colIndex - (inhibRad `div` 2)) . take (inhibRad) $ cols --TODO double check

kmaxOverlap :: Int -> [Column] -> [Column]
kmaxOverlap k cols = take k $ sortOn overlap cols -- TODO double check

maybeActivate :: Column -> [Column] -> Column
maybeActivate col cols
  | col `elem` cols = col{columnState = ActiveColumn}
  | otherwise = col{columnState = InactiveColumn}


-- learn
learn :: [Column] -> [Column]
learn col = col -- TODO



updatePermanence :: Region -> Region
updatePermanence r = r -- TODO

updateBoost :: Region -> Region
updateBoost r = r -- TODO


{-
  Temporal pooler
-}


-- TODO applie a spacialPooler on a region
temporalPooler :: Region -> Region
temporalPooler m = m
-- Represent input within the previous context
  -- For each active column -> activate predictive cells | activate all cells
-- Predict next state
  -- For cells with n nr of Active Dentrite -> Predict state
  -- Update PermenanceValue between



-- TODO
-- Clean up and improve Code
-- Add a function that creates a segment
-- Implement spacial pooler
-- Implement temporal pooler



{-

  VIEW

-}

main = do
  let sdrConfig = initSDRConfig
  let config = initConfig sdrConfig
  let region = initRegion config
  let encodedSDR = encode 50 sdrConfig
  print encodedSDR
  print region
  let region2 = spacialPooler config encodedSDR region
  print region2

-- main = print <$> spacialPooler <*> (encode 12 sdrConfig) <*> (initRegion initConfig initSDRConfig)

initConfig :: SDRConfig -> Config
initConfig sdrConfig = Config{
   nrOfColumns              = 100
   , nrOfCellsPerColumn     = 1
   , maxNrOfInputBits       = 2 -- The nr of synapses from the inputregion to a column
   , nrOfSynapsesPerCell    = 1
   , mappingType            = Random
   , initConnectionStrength = 1.0 -- on the synapses between cells in a region, should be a kernel function
   , sdrRange               = SDRRange {minIndex  = 0, maxIndex = totNrBits sdrConfig}
   , overlapThreshold = 0
   , pConthresh = 0.7
   , colActLev = 2
}

initSDRConfig :: SDRConfig
initSDRConfig = SDRConfig{
  minVal          = 0
  , maxVal        = 100
  , buckets       = 50
  , bitsPerBucket = 50
}
