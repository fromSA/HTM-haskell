-- |
-- Module      : HTM
-- Description : The Hierarchical temporal memory algorithm
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
module SRC.HTM.HTM
  ( module SRC.Region.Region,
    module SRC.SDR,
    module SRC.MovingAverage,
    spatialPooler,
    temporalPooler,
  )
where

import Control.Lens
  ( element,
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
import SRC.CommonDataTypes (Index')
import SRC.Encoder.Numeric
import SRC.HTM.Config
import SRC.MovingAverage
  ( MovingAverage (..),
    average,
    sparsity,
    off,
    on,
    window,
  )
import SRC.Package
import SRC.Region.Region
import SRC.SDR (SDR, SDRRange (..), maxIndex, minIndex, sdr)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

-- -------------------------------------------------------------
--                           UPDATE
-- -------------------------------------------------------------

{--------------------------------------------------------------
                           SpatialPooler
---------------------------------------------------------------}

-- $Spatial pooler

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
isColumnActive p syn = syn ^. conStr >= p ^. conH . spatialConfig . pConthresh && syn ^. ind `elem` p ^. value . sdr

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
kmaxOverlap k cols = take k' $ sortOn _overlap cols -- TODO test
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
  | col ^. columnState == ActiveColumn = col & inputField %~ map (learnFeedForwardSynapse p)
  | otherwise = col

-- | Activate a synapse if is connected to and active SDR bit.
learnFeedForwardSynapse :: Package -> FeedForwardSynapse -> FeedForwardSynapse
learnFeedForwardSynapse p syn =
  if synapseIsActive
    then syn & conStr %~ min 1 . (+ p ^. conH . spatialConfig . proxSynConInc)
    else syn & conStr %~ max 0 . subtract (p ^. conH . spatialConfig . proxSynConDec)
  where
    synapseIsActive = syn ^. ind `elem` p ^. value . sdr

-- | update the boostfactor of a column based of the overlap and active moving average.
updateBoost p = updateBoostFactor p . checkAvgOverlap p

-- | update the connectionstrength of the proximal synapses for a column based on its moving average overlap score.
checkAvgOverlap p col =
  if sparsity (col ^. odc) < (p ^. conH . spatialConfig . mop)
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
-- $Temporal pooler

-- | Puts the region passed from the spatial pooler in the context of the previous region,
--  then predicts the next possible next input encoding.
temporalPooler :: Package -> Region -> IO Region
temporalPooler p r = do
  a <- addContext p r
  let b = predict p a
  return $ switch b

--------------------------------------------------------------
--                           ADD CONTEXT
--------------------------------------------------------------

-- | Put a region within the context of the previous step.
addContext :: Package -> Region -> IO Region
addContext p r = do
  let prevWinnerCells = collectCells (^. isWinner) (r ^. previousStep)
  a <- activateColumns p prevWinnerCells (r ^. previousStep) $ zip (r ^. currentStep) (r ^. previousStep)
  return $ r & currentStep .~ a

-- | Update cells in columns.
activateColumns :: Package -> [Cell] -> [Column] -> [(Column, Column)] -> IO [Column]
activateColumns p pwc prev = mapM (activateColumn p pwc prev)

-- | For active columns, either activate predicted cells or burst.
-- For inactive columns, punish predicted cells.
activateColumn :: Package -> [Cell] -> [Column] -> (Column, Column) -> IO Column -- TODO clean up, you are passing in prev twice
activateColumn p pwc prev (c, pr) =
  case c ^. columnState of
    ActiveColumn -> do
      activeCells <- activateCells p pwc prev (pr ^. cells)
      return $ c & cells .~ activeCells
    InActiveColumn -> do
      return $ pr & (cells . traverse) %~ punishPredictedCell p prev

-- | Given a list of cells, either activate predicted cells or burst the cells,
-- depending on if one of the cells is predicted.
activateCells :: Package -> [Cell] -> [Column] -> [Cell] -> IO [Cell]
activateCells p pwc prev cells =
  if containsPredictiveCell cells
    then activatePredictedCells p pwc prev cells -- just the predicted cells
    else burst p pwc prev cells -- all cells

-- | Check if one of cells are predicted.
containsPredictiveCell :: [Cell] -> Bool
containsPredictiveCell = foldl (\b x -> b || x ^. cellState == PredictiveCell || x ^. cellState == ActivePredictiveCell) False

-- | Given a list of cells, activate those that are perdicted.
activatePredictedCells :: Package -> [Cell] -> [Column] -> [Cell] -> IO [Cell]
activatePredictedCells p pwc prev = mapM (activatePredictedCell p pwc prev)

-- | If a cell is predicted then activate the cell, learn on active segments and
-- maintainsparcity by growing new synapses if enough are not active.
activatePredictedCell :: Package -> [Cell] -> [Column] -> Cell -> IO Cell
activatePredictedCell p pwc prev cell
  | cell ^. cellState == PredictiveCell || cell ^. cellState == ActivePredictiveCell = do
    let learned =
          if p ^. conH . temporalConfig . learningEnabled
            then learnActiveSegments p prev (cell ^. dendrites)
            else cell ^. dendrites
    ndend <- maintainSparcityPerDendrite p pwc cell learned
    return cell {_isWinner = True, _cellState = ActiveCell, _dendrites = ndend}
  | otherwise = return cell {_isWinner = False, _cellState = InActiveCell}

-- | Grow new synpases (nrOfSynapsesPerSegment - matching synapses) on each active Segment.
maintainSparcityPerDendrite :: Package -> [Cell] -> Cell -> [Dendrite] -> IO [Dendrite]
maintainSparcityPerDendrite p pwc cell den = sequence $ den & traverse %~ \x -> sequence $ x & traverse %~ maintainSparcityPerSegment p pwc cell -- TODO extract Segment and return an IO Dendrite

-- | Grow new synapses on the segment if it is considered active.
maintainSparcityPerSegment :: Package -> [Cell] -> Cell -> Segment -> IO Segment
maintainSparcityPerSegment p pwc cell seg =
  if seg ^. segmentState == ActiveSegment
    then do
      syns <- maintainSparcity p cell pwc seg
      let newSeg = seg & synapses %~ (++ syns)
      return newSeg
    else return seg

-- | check is a cell is connected to a cell.
connectedToSeg :: Segment -> Cell -> Bool
connectedToSeg seg cell = foldl (\b x -> b || (x ^. source {-changed-}) == (cell ^. cellId)) False (seg ^. synapses)

-- | Activate all cells, learn on active segments and select a winner cell.
-- If there is a best matching segment = @Sm@ then select its assosiated cell as the winner cell,
-- else choose the least used cell as the winner cells and grow a new segment = @Sn@ on it.
-- Then on the winner cell, maintain the sparcity on @Sm@ or @Sn@, i.e. 'maintainSparcity'.
-- Bursting a column implies that the column is about to learn a new sequence of input.
burst :: Package -> [Cell] -> [Column] -> [Cell] -> IO [Cell]
burst p pwc prev cells = do
  -- Activate and learn on all cells
  let currentCells = map (\x -> x & cellState .~ ActiveCell & dendrites %~ if p ^. conH . temporalConfig . learningEnabled then learnActiveSegments p prev else id) cells

  -- Find the bestMatchingSegment
  let (cellInd, (dendInd, (segInd, matchVal))) = findBestMatching currentCells
  let [c, d, s] = map naturalToInt [cellInd, dendInd, segInd]

  -- Find the winnnerCells
  if matchVal >= (p ^. conH . temporalConfig . learningThreshold)
    then updateBestMatchingSegment p pwc c d s currentCells
    else updateLeastUsedCell p pwc currentCells

-- | Grow synapses on the bestmatchingsegment and mark the corresponding cell as the winner.
updateBestMatchingSegment :: Package -> [Cell] -> Int -> Int -> Int -> [Cell] -> IO [Cell]
updateBestMatchingSegment p pwc c d s cells = do
  -- Grow synapses on the bestmatchingsegment
  let winnerCell = cells !! c
  let bestMacthingSeg = (winnerCell ^. dendrites) !! d !! s
  -- Find winnercells from the previous iteration that are not connected to this winnercells
  newSynapses <- maintainSparcity p winnerCell pwc bestMacthingSeg

  -- Append the new synapses to bestMatchingSegment
  let winnerCell1 = winnerCell & dendrites . element d . element s . synapses %~ (++ newSynapses)

  -- Update the matchingStrength on this new cell
  let winnerCell2 = winnerCell1 & dendrites . element d . element s . matchingStrength +~ intToNatural (length newSynapses)

  -- Mark winnerCell as the winner.
  return $ cells & element c .~ (winnerCell2 & isWinner .~ True)

-- | Grow synapses on a new segment on the least used Cell, and mark the corresponding cell as the winner.
updateLeastUsedCell :: Package -> [Cell] -> [Cell] -> IO [Cell]
updateLeastUsedCell p pwc cells = do
  -- Find the least used cell
  let lc = naturalToInt $ leastUsedCell cells

  let newWinnerCell = cells !! lc

  -- Add a new segment on the least used cell
  let newSegment = growSegment
  newSynapses <- maintainSparcity p newWinnerCell pwc newSegment

  let newSegment2 =
        newSegment
          & synapses .~ newSynapses
          & matchingStrength .~ intToNatural (length newSynapses)

  -- Add the new segment with these new synapses to the winnerCell. prepend a new segment to the first dendrite in this cell.
  let winnerCell1 = newWinnerCell & dendrites %~ \xs -> (newSegment2 : head xs) : tail xs

  -- Mark winnerCell as the winner.
  return $ cells & element lc .~ (winnerCell1 & isWinner .~ True)

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
growSynapses p toCells toCell nrSynapses = do
  let nrSynapses' = naturalToInt nrSynapses
  gen <- newStdGen
  -- Problem with shuffle' -> need to make sure selectedCells in not empty.
  let len = length toCells
  let shuffledCells = if len > 0 then shuffle' toCells len gen else []
  let selectedWinnerCells = take nrSynapses' shuffledCells
  return [newSynapse (p ^. conR) toCell fromCell | fromCell <- selectedWinnerCells]

-- | Collects cells from a list of columns. A cell is collected if meets the condition f.
collectCells :: (Cell -> Bool) -> [Column] -> [Cell]
collectCells _ [] = []
collectCells f (x : xs) = l ++ ls
  where
    l = filter f (x ^. cells)
    ls = collectCells f xs

-- | Find the best matching segment from a collection of Cells
findBestMatching :: [Cell] -> (CellIndex, (DendriteIndex, (SegmentIndex, Natural)))
findBestMatching = findOne bestCell getBestDenSeg

-- | Find the best matching segment from a Cell
getBestDenSeg :: Cell -> (DendriteIndex, (SegmentIndex, Natural))
getBestDenSeg cell = findOne bestDen getBestDend (cell ^. dendrites)

-- | Find the best matching segment from a Dendrite
getBestDend :: Dendrite -> (SegmentIndex, Natural)
getBestDend = findOne bestSeg _matchingStrength

-- | Find the CellIndex of the cell that contains the best matching segment
bestCell :: (CellIndex, (DendriteIndex, (SegmentIndex, Natural))) -> (CellIndex, (DendriteIndex, (SegmentIndex, Natural))) -> (CellIndex, (DendriteIndex, (SegmentIndex, Natural)))
bestCell = select max' (snd . snd . snd)

-- | Find the DendriteIndex of the dendrite that contains the best matching segment
bestDen :: (DendriteIndex, (SegmentIndex, Natural)) -> (DendriteIndex, (SegmentIndex, Natural)) -> (DendriteIndex, (SegmentIndex, Natural))
bestDen = select max' (snd . snd)

-- | Find the SegmentIndec of the best matching segment
bestSeg :: (SegmentIndex, Natural) -> (SegmentIndex, Natural) -> (SegmentIndex, Natural)
bestSeg = select max' snd

-- | Finds one element from list of items that meets a condition f after the items are transformed by wrap.
-- Assumes there is atleast one element in ls. TODO use a Maybe Monad
findOne :: ((Natural, a) -> (Natural, a) -> (Natural, a)) -> (b -> a) -> [b] -> (Natural, a)
findOne f wrap ls =
  let (x : xs) = zip [0 ..] [wrap e | e <- ls]
   in foldl f x xs

min', max' :: Ord a => a -> a -> Bool

-- | Check if x is smaller than y
min' x y = x < y

-- | Check if x is larger than y
max' x y = x > y

-- | Select one element from a pair of items,
-- where the items are first transformed by f
-- then are compared with g
select :: (Natural -> Natural -> Bool) -> (a -> Natural) -> a -> a -> a
select g f x y = if g (f x) (f y) then x else y

-- | Find the least used cell. Incase of a tie, it currently picks the first such cell.
-- In the future, this will be break ties randomly.
leastUsedCell :: [Cell] -> CellIndex
leastUsedCell = fst . findOne leastUsed countSegments

leastUsed :: (CellIndex, Natural) -> (CellIndex, Natural) -> (CellIndex, Natural)
leastUsed = select min' snd

countSegments :: Cell -> Natural
countSegments cell = intToNatural $ sum [length den | den <- cell ^. dendrites]

--- Learn

-- | Update the '_connectionStrength' of each synapse connected to an active cell
--  on all active segments with '_permanenceIncrement' or '_permanenceDecrement'.
learnActiveSegments :: Package -> [Column] -> [Dendrite] -> [Dendrite]
learnActiveSegments p prev = map (map (learnSynapses p prev))

-- | Update the '_connectionStrength' of each synapse connected to an active cell
--  on an active segments with '_permanenceIncrement' or '_permanenceDecrement'.
learnSynapses :: Package -> [Column] -> Segment -> Segment
learnSynapses p prev seg =
  if seg ^. segmentState == ActiveSegment
    then seg & synapses %~ map (learnSynapse p prev)
    else seg

-- | Update the '_connectionStrength' of each synapse with '_permanenceIncrement'
-- if it is connected to an 'ActiveCell' or 'AtivePredictiveCell'
-- and with '_permanenceDecrement', otherwise.
learnSynapse :: Package -> [Column] -> Synapse -> Synapse
learnSynapse p prev syn =
  if preCellState == ActiveCell || preCellState == ActivePredictiveCell -- if this synpase is connected to an active cell.
    then syn & connectionStrength +~ p ^. conH . temporalConfig . permanenceIncrement
    else syn & connectionStrength -~ p ^. conH . temporalConfig . permanenceDecrement
  where
    preCellState = getCell (syn ^. source {-changed-}) prev ^. cellState

--- Punich

-- | For all synapses belonging to a cell, decrement the '_connectionStrength' with
-- '_predictedDecrement' if the '_cellState' is 'PredictiveCell'
-- and the segment is 'MatchingSegment' or 'ActiveSegment'
-- and the synapses is connected to an active cell.
-- Punishing a synapses will help the region learn from wrong predictions.
punishPredictedCell :: Package -> [Column] -> Cell -> Cell
punishPredictedCell p prev cell =
  if cell ^. cellState == PredictiveCell
    then cell & dendrites . traverse . traverse %~ punishSegment p prev
    else cell

-- | For all synapses belonging to a cell, decrement the '_connectionStrength' with
-- '_predictedDecrement' if a segment is 'MatchingSegment' or 'ActiveSegment'
-- and the synapses is connected to an active cell
punishSegment :: Package -> [Column] -> Segment -> Segment
punishSegment p prev seg =
  if seg ^. segmentState == MatchingSegment || seg ^. segmentState == ActiveSegment
    then seg & synapses . traverse %~ punishSynapse p prev
    else seg

-- | For all synapses belonging to a cell, decrement the '_connectionStrength' with
-- '_predictedDecrement' if a synapse is connected to an active cell
punishSynapse :: Package -> [Column] -> Synapse -> Synapse
punishSynapse p prev syn =
  if ActiveCell == getCell (syn ^. source {-changed-}) prev ^. cellState
    then syn & connectionStrength -~ p ^. conH . temporalConfig . predictedDecrement
    else syn

--------------------------------------------------------------
--                           Predict
--------------------------------------------------------------

predict :: Package -> Region -> Region
predict p r =
  (\x -> x & currentStep %~ map (predictCells p)) $
    updateAllSegments (updateSegmentState p . computeMatchingStrength p (r ^. previousStep) . removeDeadSynapses) r

updateAllSegments :: (Segment -> Segment) -> Region -> Region
updateAllSegments f r = r & currentStep . traverse . cells . traverse . dendrites . traverse . traverse %~ f

-------------------------------
--   Remove dead synapses    --
-------------------------------
removeDeadSynapses :: Segment -> Segment
removeDeadSynapses seg = seg & synapses %~ filter (\syn -> syn ^. connectionStrength > 0)

-------------------------------
-- Compute Matching Strength --
-------------------------------
computeMatchingStrength :: Package -> [Column] -> Segment -> Segment
computeMatchingStrength p prev seg = seg & matchingStrength .~ sumOn' (intToNatural . fromEnum . synapseIsActive p prev) (seg ^. synapses)

synapseIsActive :: Package -> [Column] -> Synapse -> Bool
synapseIsActive p prev syn =
  syn ^. connectionStrength > p ^. conH . temporalConfig . connectedPermenance
    && ActiveCell == getCell (syn ^. source {-changed-}) prev ^. cellState

-------------------------------
--      Activate Segment     --
-------------------------------
updateSegmentState :: Package -> Segment -> Segment
updateSegmentState p seg
  | seg ^. matchingStrength >= p ^. conH . temporalConfig . learningThreshold
      && seg ^. matchingStrength < p ^. conH . temporalConfig . activationThreshold =
    seg & segmentState .~ MatchingSegment
  | seg ^. matchingStrength >= p ^. conH . temporalConfig . activationThreshold = seg & segmentState .~ ActiveSegment
  | otherwise = seg & segmentState .~ InActiveSegment

-------------------------------
--      Predict Columns      --
-------------------------------

-- | If a column is predicted to be active in the next time step,
--  update the state to ActivePredicted, if it is already active.
--  Otherwise, update the state to Predicted.
predictCells :: Package -> Column -> Column
predictCells p col = col & cells %~ map (predictCell p)

-- | Helper function of predictColumn
predictCell :: Package -> Cell -> Cell
predictCell p cell =
  if predicted
    then cell & cellState %~ (\x -> if x == ActiveCell then ActivePredictiveCell else PredictiveCell)
    else cell & cellState %~ (\x -> if x == ActiveCell then x else InActiveCell)
  where
    predicted = isPredicted p cell

-- | Helper function of maybePredict
isPredicted :: Package -> Cell -> Bool
isPredicted p c = dendritesArePredicted p $ c ^. dendrites

dendritesArePredicted :: Package -> [Dendrite] -> Bool
dendritesArePredicted p = foldl (dendriteIsPredicted p) False

dendriteIsPredicted :: Package -> Bool -> [Segment] -> Bool
dendriteIsPredicted p = foldl (segmentIsPredicted p)

segmentIsPredicted :: Package -> Bool -> Segment -> Bool
segmentIsPredicted p b seg = b || seg ^. matchingStrength > p ^. conH . temporalConfig . activationThreshold

--------------------------------------------------------------
--                         Switch
---------------------------------------------------------------

-- | Move the region to the next time step.
switch :: Region -> Region
switch r = Region (r ^. previousStep) (r ^. currentStep)