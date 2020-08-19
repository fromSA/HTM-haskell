module HTM (spacialPooler, temporalPooler) where

-- MODEL
data Config = Config {
  nrOfColumns                  :: Int
  , nrOfCellsPerColumn         :: Int
  , nrOfInputBits              :: Int
  , mappingType                :: MappingType
  , inputType                  :: InputType
  , initSensoryDendritesWeight :: Float
}

-- Region

data Region = Region {
  currentStep  :: [Column],
  previousStep :: [Column]
}

data Column = Column {
  cells         :: [Cell]
  , inputField  :: [SensoryDendrite]
  , howActive   :: Float -- the average rate of activation
  , columnState :: ColumnState
}

data Cell = Cell {
  cell        :: [Segment]
  , cellState :: CellState
}
data ColumnState = ActiveColumn | InactiveColumn
data CellState = ActiveCell | InactiveCell | PredictiveCell

type Segment = [Dendrite]
type SensoryDendrite = (SensoryPixel, ConnectionStrength)
type Dendrite = [Synapse]
data Synapse = Dentrite {
  source               :: Source
  , destination        :: Destination
  , connectionStrength :: ConnectionStrength
}
type Destination = Cell
type Source = Cell
type SensoryPixel = Bool
type ConnectionStrength = Float

-- InputField

data InputField = InputField {
  portal :: [Bool]
  , kind :: InputType
}
data InputType = Circular | Linear Range
data Range = Bounded | UnBounded

-- Initialize

type Size = Int
initInputField :: Config -> InputField -- TODO incomplete cases
initInputField config =
  case inputType config of
    Circular -> InputField {
      kind = inputType config
      , portal = [] -- TODO circular case
      }
    Linear range ->
      case range of
          Bounded -> InputField { kind = inputType config
            , portal = [False | _ <- [0.. nrOfInputBits config]]
          }
          UnBounded -> InputField {
            kind = inputType config
            , portal = [] -- TODO unbounded case
            }

initRegion :: Config -> InputField -> MappingType -> Region
initRegion config input mapping = Region {
  currentStep = initColumns config input mapping
  , previousStep = initColumns config input mapping
}

initColumns :: Config -> InputField -> MappingType -> [Column]
initColumns config input mapping = [singleColumn index config input mapping | index <- [1..nrOfColumns config]]


singleColumn :: Int -> Config -> InputField -> MappingType -> Column
singleColumn index config input mapping = Column {
  cells = initCells config
  , inputField = initSensoryDendrites index input mapping
  , howActive = 0.0 -- the average rate of activation
  , columnState = InactiveColumn
}

initCells :: Config -> [Cell]
initCells config = [singleCell | _ <- [0..(nrOfCellsPerColumn config)]] -- TODO

singleCell :: Cell
singleCell = Cell {
  cell = [] -- TODO init segments
  , cellState = InactiveCell
}

initSensoryDendrites :: Int -> InputField -> MappingType -> [SensoryDendrite]
initSensoryDendrites index input mapping
  | mapping == Random = map (\x -> (x,0)) $ drop (index) $ take (index + 10) $ portal input --TODO choose random subset of inputfield portal choose init connection strength
  | mapping == SlidingWindow = map (\x -> (x,0)) $ take 10 $ portal input -- TODO choose a SlidingWindow from inputfield
-- Mapping between lists
data MappingType = Random | SlidingWindow deriving (Eq)


-- for each input[i] -> create a list of indexes from the output
type ValueMappingType = InputField -> Region -> Region
-- trivial
map1 :: ValueMappingType
map1 inputField region =
  case kind inputField of
    Circular -> region
    Linear range ->
      case range of
        Bounded   -> region -- TODO InputField $ (currentStep region) !! 0
        UnBounded -> region -- TODO


-- UPDATE

updatePermanence :: Region -> Region
updatePermanence r = r -- TODO

boost :: Region -> Region
boost r = r -- TODO

activateColumns :: InputField-> Region -> Region
activateColumns i r = r -- TODO

-- TODO applie a spacialPooler on a region
spacialPooler :: InputField-> Region -> Region
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
