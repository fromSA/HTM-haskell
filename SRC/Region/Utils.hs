-- |
-- Module      : Utils
-- Description : Utility functions for manupulating a region.
-- Copyright   : (c) Fromsa Hera, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Given a 'HTM.Region.Config', this module provides functions for constructing a region.
-- It also provides functions for accessing and modifing the region.
module SRC.Region.Utils
  ( initRegion,
    getRandomCell,
    getCell,
    newSynapse,
  )
where

import Control.Lens ((^.))
import Debug.Trace ()
import GHC.Natural (Natural, intToNatural, naturalToInt)
import SRC.CommonDataTypes (BitIndex)
import SRC.Encoder.Config
import SRC.Encoder.Numeric (getRange)
import SRC.MovingAverage (MovingAverage (..))
import SRC.Region.Config
import SRC.Region.Model
import SRC.SDR
import System.Random (Random (randomR), getStdRandom)

-- -------------------------------------------------------------
--                           GETTER
-- -------------------------------------------------------------

-- | Returns the cell within a region that matches the provided cellid.
getCell :: CellID -> [Column] -> Cell
getCell id cols = ((cols !! colId) ^. cells) !! cellId
  where
    cellId = naturalToInt (id ^. cell)
    colId = naturalToInt (id ^. col)

-- | Returns a randomly selected cell from the region and excludes the provided cell.
-- This function returns an IO Cell monad because it uses the StdRandom as a random generator.
getRandomCell :: RegionConfig -> Cell -> [Column] -> IO Cell
getRandomCell conR notCell columns = do
  randColumnIndex <- naturalToInt <$> getRandomIndexBetween _START_INDEX (conR ^. nrOfColumns)
  randCellIndex <- naturalToInt <$> getRandomIndexBetween _START_INDEX (conR ^. nrOfCellsPerColumn)
  let randCell = ((columns !! randColumnIndex) ^. cells) !! randCellIndex
  if randCell == notCell -- the cell is always the same at the beginning. Needs indexing
    then getRandomCell conR notCell columns
    else return randCell

-- | Returns a list of random SDR indecies from the SDR range defined by the encoder configuration.
selectRandomIndecies :: EncoderConfig -> RegionConfig -> [IO BitIndex]
selectRandomIndecies conS conR = maybe [] (randIndecies (conR ^. initNrOfFeedForwardSynpases)) (getRange conS)

-- | Returns n random bit indecies from an sdr range.
randIndecies :: Natural -> SDRRange -> [IO BitIndex]
randIndecies n sR
  | n <= 0 = []
  | n > 0 = randIndex sR : randIndecies (n -1) sR -- TODO double check that no duplicates occure

-- | Returns a random bit index from an sdr range.
randIndex :: SDRRange -> IO BitIndex
randIndex cR = getRandomIndexBetween (cR ^. minIndex) (cR ^. maxIndex)

-- | Returns a random value between two values.
getRandomIndexBetween :: Natural -> Natural -> IO BitIndex
getRandomIndexBetween mi ma = do
  let mii = naturalToInt mi
  let mai = naturalToInt ma
  intToNatural <$> getStdRandom (randomR (mii, mai)) -- returns an IO BitIndex

-- -------------------------------------------------------------
--                           INITILIZE
-- -------------------------------------------------------------

{-__initRegion :: Package -> Region
__initRegion p = Region
      { _currentStep = head regions,
        _previousStep = head . tail $ regions
      }
      where 
        regions = replicate 2 region
        region = initAllDendrites p $ initColumns p-}

-- | Construct a new region. 
-- This function returns an IO Region monad because it uses the StdRandom as a random generator.
initRegion :: EncoderConfig -> RegionConfig -> IO Region
initRegion conS conR = do
  region <- initAllDendrites conR $ initColumns conS conR
  let regions = replicate 2 region -- make a copy of region
  return
    Region
      { _currentStep = head regions,
        _previousStep = head . tail $ regions
      }

-- | Construct all columns in a region.
initColumns :: EncoderConfig -> RegionConfig -> IO [Column]
initColumns conS conR = mapM (initsingleColumn conS conR) [_START_INDEX .. conR ^. nrOfColumns]

-- | Construct a column.
initsingleColumn :: EncoderConfig -> RegionConfig -> Natural -> IO Column
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
            _boost = _INIT_BOOST, -- should maybe be Float
            _overlap = _INIT_OVERLAP,
            _inhibRad = _INIT_RAD -- how to select!
          }
  return c

-- | Construct all cells in a columns.
initCells :: RegionConfig -> Natural -> [Cell]
initCells conR colIndex = [singleCell colIndex cellIndex | cellIndex <- [_START_INDEX .. (conR ^. nrOfCellsPerColumn)]]

-- | Construct a cell
singleCell :: Natural -> Natural -> Cell
singleCell colIndex cellIndex =
  Cell
    { _cellId = CellID {_col = colIndex, _cell = cellIndex},
      _dendrites = [], -- The dendrites are initilized after all cells are initilized.
      _cellState = InActiveCell,
      _isWinner = False
    }

-- | Construct a list of Dendrite for all columns.
initAllDendrites :: RegionConfig -> IO [Column] -> IO [Column]
initAllDendrites conR columns = do
  cols <- columns
  mapM (initDendritesPerColumn conR cols) cols

-- | Construct a list of Dendrite for a column.
initDendritesPerColumn :: RegionConfig -> [Column] -> Column -> IO Column
initDendritesPerColumn conR columns column = do
  cells <- mapM (initDendritesPerCell conR columns) (column ^. cells)
  return
    column
      { _cells = cells
      }

-- | Construct a list of dendrites for a cell.
initDendritesPerCell :: RegionConfig -> [Column] -> Cell -> IO Cell
initDendritesPerCell conR columns cell = do
  initDend <- initDendrites conR cell columns
  return
    cell
      { _dendrites = initDend
      }

-- | Construct a list of dendrites.
initDendrites :: RegionConfig -> Cell -> [Column] -> IO [Dendrite]
initDendrites conR cell columns = do
  syns <- mapM (initSynapse conR cell columns) [_START_INDEX .. (conR ^. nrOfSynapsesPerSegment)]
  let segm =
        Segment
          { _segmentState = InActiveSegment,
            _synapses = syns,
            _matchingStrength = 0
          }
  return [[segm]] -- _nrOfSynapsesPerSegment synapses in one segment in one dendrite

-- | Construct a synapse.
initSynapse :: RegionConfig -> Cell -> [Column] -> Natural -> IO Synapse -- TODO create synapse connection to the _previousStep from the CurrentStep
initSynapse conR destCell columns _ = do
  cell <- getRandomCell conR destCell columns
  let syn = newSynapse conR destCell cell
  return syn

-- | Create a new synapse.
--
-- >>> let conR = RegionConfig 1 1 1 1 Random 0.7 1
-- >>> let from = Cell (CellID 1 1) [] InActiveCell False
-- >>> let to = Cell (CellID 2 1) [] InActiveCell False
-- >>> newSynapse conR from to
-- Synapse {_source = CellID {_col = 2, _cell = 1}, _destination = CellID {_col = 1, _cell = 1}, _connectionStrength = 0.7}
newSynapse :: RegionConfig -> Cell -> Cell -> Synapse -- TODO this function has commonality with initSynapses. Abtract it.
newSynapse conR toCell fromCell =
  Synapse
    { _source = fromCell ^. cellId,
      _destination = toCell ^. cellId,
      _connectionStrength = conR ^. initConnectionStrength --TODO should be a distribution around a center
    }

-- | Construct feedforward synapses.
initFeedForwardSynapses :: EncoderConfig -> RegionConfig -> IO [FeedForwardSynapse]
initFeedForwardSynapses conS conR = mapM (singleFeedForwardSynapse conR) $ selectRandomIndecies conS conR -- FIXME this is a list of the synapses

-- | Construct a single feedforward synapse.
singleFeedForwardSynapse :: RegionConfig -> IO BitIndex -> IO FeedForwardSynapse
singleFeedForwardSynapse config index = do
  indexVal <- index
  let f =
        FeedForwardSynapse
          { _ind = indexVal,
            _conStr = config ^. initConnectionStrength
          }
  return f

-- -------------------------------------------------------------
--                           CONSTANTS
-- -------------------------------------------------------------

-- | A constant value, represents the start index in list of items.
_START_INDEX :: Num a => a
_START_INDEX = 0

-- | A constant value, represents the initial boost value of a column.
_INIT_BOOST :: Num a => a
_INIT_BOOST = 1

-- | A constant value, represents the initial radius of a columns neighbourhood.
_INIT_RAD :: Num a => a
_INIT_RAD = 5

-- | A constant value, represents the initial value of a columns overlap score.
_INIT_OVERLAP :: Num a => a
_INIT_OVERLAP = 0