{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : HTM
-- Description : Short description
-- Copyright   : (c) Fromsa Hera, 2020
--                   Numenta, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module has to two functions 'spatialPooler' and 'temporalPooler'.
-- In addition, it contains configuration records for the functions.
-- These functions update a 'Region' in order.
-- It is an inspired by the sudo code from Numentas [Spatial Pooler](https://numenta.com/resources/biological-and-machine-intelligence/spatial-pooling-algorithm/)
-- and [Temporal Pooler](https://numenta.com/assets/pdf/temporal-memory-algorithm/Temporal-Memory-Algorithm-Details.pdf). In addition [this paper](https://arxiv.org/pdf/1601.06116.pdf) for the updating the 'boostingfactor'.
-- Here is a longer description of this module, containing some
-- commentary with @some markup@. TODO
module HTM.HTM
  ( module HTM.Region,
    module HTM.SDR,
    module HTM.MovingAverage,
    spatialPooler,
    temporalPooler,
    Package (..),
    HTMConfig (..),
    TemporalConfig (..),
    SpatialConfig (..),
  )
where

import Control.Lens
  ( element,
    makeLenses,
    (%~),
    (&),
    (+~),
    (-~),
    (.~),
    (^.),
  )
import Control.Monad ()
import Data.List.Extra (sortOn, sumOn')
import Debug.Trace (traceShow, traceShowId)
-- TODO remove
import GHC.Natural (Natural, intToNatural, naturalToInt)
import HTM.CommonDataTypes (Index')
import HTM.MovingAverage
import HTM.Region
import HTM.SDR (SDR, SDRConfig (..), SDRRange (..), encode, totNrBits)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

-- -------------------------------------------------------------
--                           CONFIG
-- -------------------------------------------------------------

-- | The parameters for the feedforward synapses that connect the inputField with a region and spatial algorithm.
data SpatialConfig = SpatialConfig
  { -- | The threshold for column activation. If the number of active bits in the inputfield of a column >= this threshold, then the column becomes active.
    _overlapThreshold :: Natural,
    -- | The minimum percent of active bits in inputField expected to have a overlap with.
    _mop :: Float,
    -- | The amount to decrease the connection strength of a synapses with for proximal synapses
    _proxSynConInc :: Float,
    -- | The amount to decrease the connection strength of a synapses with for proximal synapses
    _proxSynConDec :: Float,
    -- | A synapse with connection strength >= this threshold is considered permenantly connected.
    _pConthresh :: Float,
    -- | The desired column activity level within inhibition radius i.e. the number of columns in that should be activated within a activaiton radius.
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

-- | A package record containing the configaration parameters and the sdr for the current time step. This is used for the simplication of the function types.
data Package = Package
  { -- | The configuration parameters for the HTM algorithm.
    _conH :: HTMConfig,
    -- | The configuration parameters for a Region.
    _conR :: RegionConfig,
    -- | The configuration parameters for the SDR encoding.
    _conS :: SDRConfig,
    -- | The SDR encoding of the current value.
    _sdr :: SDR
  }

-- | Lenses for HTMConfig, used to navigate the record.
makeLenses ''Package

-- -------------------------------------------------------------
--                           UPDATE
-- -------------------------------------------------------------

{--------------------------------------------------------------
                           SpatialPooler
---------------------------------------------------------------}

-- | The spatial pooler, used to spatially encode an input SDR on a Region.
spatialPooler :: Package -> Region -> Region
spatialPooler p r = r & currentStep %~ (learn p . updateInhibition p . updateOverlap p)

learn, updateInhibition, updateOverlap :: Package -> [Column] -> [Column]
computeOverlap, learnCol, updateBoost, updateBoostFactor, checkAvgOverlap :: Package -> Column -> Column

---------------------
-- COMPUTE OVERLAP --
---------------------

-- | Updates the overlap score of all columns.
updateOverlap p = map (computeOverlap p)

-- | Updates the overlap score of a column
computeOverlap p c =
  if overlapScore >= p ^. conH . spatialConfig . overlapThreshold
    then -- boost the overlap score, based on the '_boost' factor at the previous iteration.

      c & overlap .~ floor (c ^. boost * fromIntegral overlapScore)
        & odc %~ on
    else
      c & overlap .~ overlapScore
        & odc %~ off
  where
    -- Counts the how many synapses in the inputfield of a column are active
    overlapScore = intToNatural $ sum $ map (fromEnum . isColumnActive p) (c ^. inputField)

-- | Choose if a column is active or not based on if
--  the connection strength of the Synapse to the input space is larger than
--  the activation threshold
isColumnActive :: Package -> FeedForwardSynapse -> Bool
isColumnActive p syn = syn ^. conStr >= p ^. conH . spatialConfig . pConthresh && syn ^. ind `elem` p ^. sdr

---------------------
--   Inhibition    --
---------------------

-- | Inhibit columns that do not have an overlap score among the k highest
updateInhibition p columns = map (maybeActivateColumn p columns) columns

-- | A Column is activated if the it is one the k columns with the  highest activaiton function.
maybeActivateColumn :: Package -> [Column] -> Column -> Column
maybeActivateColumn p cols col =
  maybeActivate col . kmaxOverlap (p ^. conH . spatialConfig . colActLev) $ neighbors col cols

-- | Returns the neighbors of column within a radius. The radius is clipped if the column is at the edge of the region. -- TODO perhaps use a rotational way?
neighbors :: Column -> [Column] -> [Column]
neighbors col = neighbors' (col ^. columnId) (col ^. inhibRad)

-- | Returns the elements within a radious of an index from a list.
neighbors' :: Natural -> Natural -> [a] -> [a]
neighbors' at rad = take rad' . drop (at' - (rad' `div` 2))
  where
    rad' = naturalToInt rad
    at' = naturalToInt at

-- | Returns k columns with the highest overlapscore from a list of columns.
kmaxOverlap :: Natural -> [Column] -> [Column]
kmaxOverlap k cols = take k' $ sortOn _overlap cols -- TODO double check
  where
    k' = naturalToInt k

-- | Activate a column if it is in a list of columns, else Inactivate it.
maybeActivate :: Column -> [Column] -> Column
maybeActivate col cols
  | col `elem` cols = col & columnState .~ ActiveColumn & adc %~ on
  | otherwise = col & columnState .~ InActiveColumn & adc %~ off

---------------------
--      LEARN      --
---------------------

-- | Update the connection strength of synapses within a region and update '_boost' of all columns.
learn p = map (updateBoost p . learnCol p) -- TODO might be a problem if the connection strength of currentStep and prevStep are different! Maybe separate the synapses from the columns

-- | Update the connection strength of synapses within a region
learnCol p col
  | col ^. columnState == ActiveColumn = col & inputField %~ map (activateSynapse p)
  | otherwise = col

activateSynapse :: Package -> FeedForwardSynapse -> FeedForwardSynapse
activateSynapse p syn =
  if synapseIsActive
    then syn & conStr %~ min 1 . (+ p ^. conH . spatialConfig . proxSynConInc)
    else syn & conStr %~ max 0 . subtract (p ^. conH . spatialConfig . proxSynConDec)
  where
    synapseIsActive = syn ^. ind `elem` p ^. sdr

updateBoost p = updateBoostFactor p . checkAvgOverlap p

checkAvgOverlap p col =
  if averagePercent (col ^. odc) < (p ^. conH . spatialConfig . mop)
    then col & (inputField . traverse . conStr) %~ (min 1 . (+ 0.1 {- arbitrary value, should be a parameter.-} * p ^. conH . spatialConfig . pConthresh))
    else col

-- | Updates the `_boost` factor of a column.
-- TODO update _boost, double check that _boost is supposed to be used.
updateBoostFactor p col = col & boost %~ (boostFactor *)
  where
    boostFactor = exp $ - p ^. conH . temporalConfig . boostStrength * (shift - center)
    shift = average $ col ^. adc
    center = p ^. conH . temporalConfig . targetDensity * fromIntegral (col ^. adc . window)

{--------------------------------------------------------------
                           Temporal Pooler
---------------------------------------------------------------}

-- TODO apply the temporal pooler algorithm on a region
temporalPooler :: Package -> Region -> IO Region
temporalPooler p r = do
  a <- addContext p r
  let b = predict p a
  return $ switch b

--------------------------------------------------------------
--                           ADD CONTEXT
--------------------------------------------------------------

addContext :: Package -> Region -> IO Region
addContext p r = do
  let prevWinnerCells = collectCells (^. isWinner) (r ^. previousStep)
  a <- activateColumns p prevWinnerCells (r ^. previousStep) $ zip (r ^. currentStep) (r ^. previousStep)
  return $ r & currentStep .~ a

activateColumns :: Package -> [Cell] -> [Column] -> [(Column, Column)] -> IO [Column]
activateColumns p pwc prev = mapM (activateColumn p pwc prev)

activateColumn :: Package -> [Cell] -> [Column] -> (Column, Column) -> IO Column -- TODO clean up, you are passing in prev twice
activateColumn p pwc prev (c, pr) =
  case c ^. columnState of
    ActiveColumn -> do
      activeCells <- activateCells p pwc prev (pr ^. cells)
      return $ c & cells .~ activeCells
    InActiveColumn -> do
      return $ pr & (cells . traverse) %~ punishPredictedCell p prev

activateCells :: Package -> [Cell] -> [Column] -> [Cell] -> IO [Cell]
activateCells p pwc prev cells =
  if containsPredictiveCell cells
    then activatePredictedCells p pwc prev cells -- just the predicted cells
    else burst p pwc prev cells -- all cells

containsPredictiveCell :: [Cell] -> Bool
containsPredictiveCell = foldl (\b x -> b || x ^. cellState == PredictiveCell || x ^. cellState == ActivePredictiveCell) False

activatePredictedCells :: Package -> [Cell] -> [Column] -> [Cell] -> IO [Cell]
activatePredictedCells p pwc prev = mapM (activatePredictedCell p pwc prev)

activatePredictedCell :: Package -> [Cell] -> [Column] -> Cell -> IO Cell
activatePredictedCell p pwc prev cell
  | cell ^. cellState == PredictiveCell || cell ^. cellState == ActivePredictiveCell = do
    ndend <- maintainSparcityPerDendrite p pwc cell $ learnActiveSegments p prev (cell ^. dendrites)
    return cell {_isWinner = True, _cellState = ActiveCell, _dendrites = ndend}
  | otherwise = return cell {_isWinner = False, _cellState = InActiveCell}

-- | Grow new synpases (nrOfSynapsesPerSegment - matching synapses) on each active Segment.
maintainSparcityPerDendrite :: Package -> [Cell] -> Cell -> [Dendrite] -> IO [Dendrite]
maintainSparcityPerDendrite p pwc cell den = sequence $ den & traverse %~ \x -> sequence $ x & traverse %~ maintainSparcityPerSegment p pwc cell -- TODO extract Segment and return an IO Dendrite

maintainSparcityPerSegment :: Package -> [Cell] -> Cell -> Segment -> IO Segment
maintainSparcityPerSegment p pwc cell seg =
  if seg ^. matchingStrength >= p ^. conH . temporalConfig . activationThreshold
    then do
      syns <- maintainSparcity p cell pwc seg
      let newSeg = seg & synapses %~ (++ syns)
      return newSeg
    else return seg

connectedToSeg :: Segment -> Cell -> Bool
connectedToSeg seg cell = foldl (\b x -> b || (x ^. destination) == (cell ^. cellId)) False (seg ^. synapses)

burst :: Package -> [Cell] -> [Column] -> [Cell] -> IO [Cell]
burst p pwc prev cells = do
  -- Activate and learn on all cells
  let currentCells = map (\x -> x & cellState .~ ActiveCell & dendrites %~ if p ^. conH . temporalConfig . learningEnabled then learnActiveSegments p prev else id) cells

  -- Find the bestMatchingSegment
  let (cellInd, (dendInd, (segInd, matchVal))) = findBestMatching p prev currentCells
  let [c, d, s] = map naturalToInt [cellInd, dendInd, segInd]
  -- Find the winnnerCellsmain

  if matchVal >= (p ^. conH . temporalConfig . learningThreshold)
    then -- grow synapses on the bestmatchingsegment
    do
      let newWinnerCell = currentCells !! c
      let bestMacthingSeg = (newWinnerCell ^. dendrites) !! d !! s
      -- Find winnercells from the previous iteration that are not connected to this winnercells

      newSynapses <- maintainSparcity p newWinnerCell pwc bestMacthingSeg

      -- Append the new synapses to bestMatchingSegment
      let newWinnerCell1 = newWinnerCell & dendrites . element d . element s . synapses %~ (++ newSynapses)
      let newWinnerCell2 = newWinnerCell1 & dendrites . element d . element s . matchingStrength +~ intToNatural (length newSynapses)
      -- Mark winnerCell as the winner.
      let newWinnerCell3 = newWinnerCell2 & isWinner .~ True

      return $ currentCells & element c .~ newWinnerCell3
    else -- grow synapses on a new segment on the least used Cell
    do
      -- Find the least used cell
      let lc = naturalToInt $ leastUsedCell currentCells
      -- Add a new segment on the least used cell
      let newWinnerCell = currentCells !! lc

      let newSegment = growSegment
      newSynapses <- maintainSparcity p newWinnerCell pwc newSegment
      let newSegment2 =
            newSegment
              & synapses .~ newSynapses
              & matchingStrength .~ intToNatural (length newSynapses)

      -- Add a segment with these new synapses to the winnerCell. Prepends a new segment to the first dendrite in this cell.
      let newWinnerCell1 = newWinnerCell & dendrites %~ \xs -> (newSegment2 : head xs) : tail xs

      -- Mark winnerCell as the winner.
      let newWinnerCell2 = newWinnerCell1 & isWinner .~ True

      return $ currentCells & element lc .~ newWinnerCell2

maintainSparcity :: Package -> Cell -> [Cell] -> Segment -> IO [Synapse]
maintainSparcity p cell pwc seg = do
  -- Find winnercells from the previous iteration that are not connected to this winnercells
  let unConnectedPrevWinnerCells = filter (not . connectedToSeg seg) pwc
  -- grow synapses from the newWinnerCell to the winnercells from the previous iteration
  let nrOfNewSynapses =
        if (p ^. conR . nrOfSynapsesPerSegment) > (seg ^. matchingStrength)
          then (p ^. conR . nrOfSynapsesPerSegment) - (seg ^. matchingStrength)
          else p ^. conR . nrOfSynapsesPerSegment
  -- grow synapses from the newWinnerCell to the winnercells from the previous iteration
  growSynapses p unConnectedPrevWinnerCells cell nrOfNewSynapses

growSegment :: Segment
growSegment = Segment {_segmentState = InActiveSegment, _synapses = [], _matchingStrength = 0}

--  Grow new synapses. TODO what happens if there are not enough winnercells?
growSynapses :: Package -> [Cell] -> Cell -> Index' -> IO [Synapse]
growSynapses p toCells fromCell nrSynapses = do
  let nrSynapses' = naturalToInt nrSynapses
  gen <- newStdGen
  -- Problem with shuffle' -> need to make sure selectedCells in not empty.
  let len = length toCells
  let shuffledCells = if len > 0 then shuffle' toCells len gen else []
  let selectedWinnerCells = take nrSynapses' shuffledCells
  return [newSynapse p fromCell wCell | wCell <- selectedWinnerCells]

newSynapse :: Package -> Cell -> Cell -> Synapse -- TODO this function has commonality with initSynapses. Abtract it.
newSynapse p source dest =
  Synapse
    { _source = source ^. cellId,
      _destination = dest ^. cellId,
      _connectionStrength = p ^. conR . initConnectionStrength --TODO should be a distribution around a center
    }

--collectCells :: (Cell -> Bool) -> [Column] -> [Cell] -- TODO there is something wrong with this.
collectCells :: (Cell -> Bool) -> [Column] -> [Cell]
collectCells _ [] = []
collectCells f (x : xs) = l ++ ls
  where
    l = filter f (x ^. cells)
    ls = collectCells f xs

---

connectedTo :: Cell -> Cell -> Bool
connectedTo from to = foldl (foldDend from) False (to ^. dendrites)

foldDend :: Cell -> Bool -> Dendrite -> Bool
foldDend cell = foldl (foldSeg cell)

foldSeg :: Cell -> Bool -> Segment -> Bool
foldSeg cell b seg = foldl (checkConnection cell) b (seg ^. synapses)

checkConnection :: Cell -> Bool -> Synapse -> Bool
checkConnection cell b syn = b || (syn ^. destination) == (cell ^. cellId)

--- find the best matching segment
findBestMatching :: Package -> [Column] -> [Cell] -> (CellIndex, (DendriteIndex, (SegmentIndex, Natural)))
findBestMatching p prev = findOne bestCell (getBestDenSeg p prev)

getBestDenSeg :: Package -> [Column] -> Cell -> (DendriteIndex, (SegmentIndex, Natural))
getBestDenSeg p prev cell = findOne bestDen (getBestDend p prev) (cell ^. dendrites)

getBestDend :: Package -> [Column] -> Dendrite -> (SegmentIndex, Natural)
getBestDend p prev = findOne bestSeg _matchingStrength

bestCell :: (CellIndex, (DendriteIndex, (SegmentIndex, Natural))) -> (CellIndex, (DendriteIndex, (SegmentIndex, Natural))) -> (CellIndex, (DendriteIndex, (SegmentIndex, Natural)))
bestCell = select max' (snd . snd . snd)

bestDen :: (DendriteIndex, (SegmentIndex, Natural)) -> (DendriteIndex, (SegmentIndex, Natural)) -> (DendriteIndex, (SegmentIndex, Natural))
bestDen = select max' (snd . snd)

bestSeg :: (SegmentIndex, Natural) -> (SegmentIndex, Natural) -> (SegmentIndex, Natural)
bestSeg = select max' snd

-- Assumes there is atleast one element in ls. TODO use a Maybe Monad
findOne :: ((Natural, a) -> (Natural, a) -> (Natural, a)) -> (b -> a) -> [b] -> (Natural, a)
findOne f wrap ls =
  let (x : xs) = zip [0 ..] [wrap e | e <- ls]
   in foldl f x xs

min', max' :: Ord a => a -> a -> Bool
min' x y = x < y
max' x y = x > y

select :: (Natural -> Natural -> Bool) -> (a -> Natural) -> a -> a -> a
select g f x y = if g (f x) (f y) then x else y

-- find the least used cell
leastUsedCell :: [Cell] -> CellIndex
leastUsedCell = fst . findOne leastUsed countSegments

leastUsed :: (CellIndex, Natural) -> (CellIndex, Natural) -> (CellIndex, Natural)
leastUsed = select min' snd

countSegments :: Cell -> Natural
countSegments cell = intToNatural $ sum [length den | den <- cell ^. dendrites]

--- Learn

learnActiveSegments :: Package -> [Column] -> [Dendrite] -> [Dendrite]
learnActiveSegments p prev = map (map (learnSynapses p prev))

learnSynapses :: Package -> [Column] -> Segment -> Segment
learnSynapses p prev seg =
  if seg ^. segmentState == ActiveSegment
    then seg & synapses %~ map (learnSynapse p prev)
    else seg

learnSynapse :: Package -> [Column] -> Synapse -> Synapse
learnSynapse p prev syn =
  if preCellState == ActiveCell || preCellState == ActivePredictiveCell -- if this synpase is connected to an active cell.
    then syn & connectionStrength +~ p ^. conH . temporalConfig . permanenceIncrement
    else syn & connectionStrength -~ p ^. conH . temporalConfig . permanenceDecrement
  where
    preCellState = getCell (syn ^. destination) prev ^. cellState

--- Punich

punishPredictedCell :: Package -> [Column] -> Cell -> Cell
punishPredictedCell p prev cell =
  if cell ^. cellState == PredictiveCell
    then cell & dendrites . traverse . traverse %~ punishSegment p prev
    else cell

punishSegment :: Package -> [Column] -> Segment -> Segment
punishSegment p prev seg =
  if seg ^. segmentState == MatchingSegment || seg ^. segmentState == ActiveSegment
    then seg & synapses . traverse %~ punishSynapse p prev
    else seg

punishSynapse :: Package -> [Column] -> Synapse -> Synapse
punishSynapse p prev syn =
  if ActiveCell == getCell (syn ^. destination) prev ^. cellState
    then syn & connectionStrength -~ (p ^. conH . temporalConfig . predictedDecrement)
    else syn

--------------------------------------------------------------
--                           Predict
--------------------------------------------------------------

predict :: Package -> Region -> Region
predict p r =
  (\x -> x & currentStep %~ map (predictColumn p (r ^. previousStep))) $
    updateAllSegments (updateSegmentState p . computeMatchingStrength p (r ^. previousStep)) r -- . removeDeadSynapses)

updateAllSegments :: (Segment -> Segment) -> Region -> Region
updateAllSegments f r = r & currentStep . traverse . cells . traverse . dendrites . traverse . traverse %~ f

-------------------------------
-- Compute Matching Strength --
-------------------------------
computeMatchingStrength :: Package -> [Column] -> Segment -> Segment
computeMatchingStrength p prev seg = seg & matchingStrength .~ sumOn' (intToNatural . fromEnum . synapseIsActive p prev) (seg ^. synapses)

synapseIsActive :: Package -> [Column] -> Synapse -> Bool
synapseIsActive p prev syn =
  syn ^. connectionStrength > p ^. conH . temporalConfig . connectedPermenance
    && ActiveCell == getCell (syn ^. destination) prev ^. cellState

-------------------------------
--   Remove dead synapses    --
-------------------------------
removeDeadSynapses :: Segment -> Segment
removeDeadSynapses seg = seg & synapses %~ filter (\syn -> syn ^. connectionStrength >= 0)

-------------------------------
--      Activate Segment     --
-------------------------------
updateSegmentState :: Package -> Segment -> Segment
updateSegmentState p seg
  | seg ^. matchingStrength > p ^. conH . temporalConfig . activationThreshold = seg & segmentState .~ ActiveSegment
  | seg ^. matchingStrength > p ^. conH . temporalConfig . learningThreshold = seg & segmentState .~ MatchingSegment
  | otherwise = seg & segmentState .~ InActiveSegment

-------------------------------
--      Predict Columns      --
-------------------------------

-- | If a column is predicted to be active in the next time step,
--  update the state to ActivePredicted, if it is already active.
--  Otherwise, update the state to Predicted.
predictColumn :: Package -> [Column] -> Column -> Column
predictColumn p prev col = col & cells %~ map (maybePredict p prev)

-- | Helper function of predictColumn
maybePredict :: Package -> [Column] -> Cell -> Cell
maybePredict p prev cell =
  if predicted
    then cell & cellState %~ (\x -> if x == ActiveCell then ActivePredictiveCell else PredictiveCell)
    else cell & cellState %~ (\x -> if x == ActiveCell then x else InActiveCell)
  where
    predicted = isPredicted p prev cell

-- | Helper function of maybePredict
isPredicted :: Package -> [Column] -> Cell -> Bool
isPredicted p prev c = dendriteIsPredicted p prev $ c ^. dendrites

dendriteIsPredicted :: Package -> [Column] -> [Dendrite] -> Bool
dendriteIsPredicted p prev = foldl (segmentIsPredicted p prev) False

segmentIsPredicted :: Package -> [Column] -> Bool -> [Segment] -> Bool
segmentIsPredicted p prev = foldl (synapceIsPredicted p prev)

synapceIsPredicted :: Package -> [Column] -> Bool -> Segment -> Bool
synapceIsPredicted p prev b seg = b || seg ^. matchingStrength > p ^. conH . temporalConfig . activationThreshold

--------------------------------------------------------------
--                         Switch
---------------------------------------------------------------

-- | Move the region to the next time step.
switch :: Region -> Region
switch r = Region (r ^. previousStep) (r ^. currentStep)