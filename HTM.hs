  {-# LANGUAGE TemplateHaskell #-}
{-|
Module      : HTM
Description : Short description
Copyright   : (c) Fromsa Hera, 2020
                  Numenta, 2020
License     : GPL-3
Maintainer  : fromsahera28@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module HTM where

import           System.Random
import           System.Random.Shuffle
import           MovingAverage 
import           SDR           (SDR, SDRConfig(..), SDRRange(..), encode, totNrBits)
import           Region
import           Data.List.Extra (intercalate, sortOn, sumOn')
import Control.Lens
-- -------------------------------------------------------------
--                           CONFIG
-- -------------------------------------------------------------

-- |The configuration parameters for the HTM algorithm.
data HTMConfig = HTMConfig{
  _overlapThreshold         :: Int -- ^ The threshold for column activation. If the number of active bits in the inputfield of a column >= this threshold, then the column becomes active.
  , _pConthresh             :: Float -- ^ A synapse with connection strength >= this threshold is considered permenantly connected.
  , _colActLev              :: Int -- ^ The desired column activity level within inhibition radius i.e. the number of columns in that should be activated within a activaiton radius.
  , _proxSynConInc :: Float -- ^The amount to decrease the connection strength of a synapses with for proximal synapses 
  , _proxSynConDec :: Float -- ^The amount to decrease the connection strength of a synapses with for proximal synapses 
  , _mop :: Float -- ^The minimum percent of active bits in inputField expected to have a overlap with.
  , _boostStrength :: Float -- ^The strength of boost value between about 0
  , _targetDensity :: Float -- ^The desired percent of active duty cycle within the sliding window.
  , _connectedPermenance :: Float -- ^The threshold of permenant synaptic connection. If a synapse has higher connection than this threshold and is connected to an active cell, then it is considered active. 
  , _activationThreshold :: Int -- ^The number of synapses per segment that must be active for the segment to be considered active. should be less than nrOfSynapsesPerSegment of RegionConfig
  , _predictedDecrement :: Float 
  , _permanenceDecrement :: Float -- ^The forgetting strength, detrmines how fast a pattern is forgotten by the region.
  , _permanenceIncrement :: Float -- ^The learning strength, determines how fast a pattern is learned by the region
  , _matchingThreshold :: Int -- ^The threshold for when a cell is considered matching, i.e. when a segment has this many activeconnections to the previously active/winner cells.
  , _learningEnabled :: Bool -- ^True if the temporalPooler should update the connectionstrengths of the synapses.

}

makeLenses ''HTMConfig

-- -------------------------------------------------------------
--                           UPDATE
-- -------------------------------------------------------------


{--------------------------------------------------------------
                           SpatialPooler
---------------------------------------------------------------}


spacialPooler :: HTMConfig -> SDR -> Region -> Region
spacialPooler conH sdr region = region & currentStep %~ (learn conH sdr . updateInhibition conH . updateOverlap conH sdr)
  

-- initialize
    -- first map input to region -- DONE
-- phase1 : overlap
    -- activate columns based on their input field
-- phase2 : Inhibition
    -- boost the column (to maintain a fixed sparcity)
-- phase 3 : learning
    -- update permenance values of each Sensory


--activateColumns config sdr region = region { currentStep = learn . (updateInhibition config) $ updateOverlap config sdr (currentStep region)}
-- for each column -> compute overlap & boost it by the boost factor
-- for each column -> activate columns if the column is not inhibited
-- Learning: for each active column -> update permanence
-- UpdateBoosting: for each columns ->
  -- update boosting/
  -- update active duty & overlap cycle
  -- update inhibition radius



--    COMPUTE OVERLAP
-----------------------


-- |Updates the overlap score of all columns
updateOverlap :: HTMConfig -> SDR -> [Column] -> [Column]
updateOverlap conH sdr = map (computeOverlap conH sdr)

-- |Updates the overlap score of a column
computeOverlap :: HTMConfig -> SDR -> Column -> Column
computeOverlap conH sdr column =
  if overlap >= conH^.overlapThreshold
    then column{_overlap = floor $ fromIntegral overlap  *  column^.boost, _odc = on (column^.odc)}
    else column{_overlap = 0 , _odc = off ( column^.odc)}
  where
    overlap = countOverlap conH sdr (column^.inputField)


-- |Counts the how many synapses in the inputfield of a column are active 
countOverlap :: HTMConfig -> SDR -> [FeedForwardSynapse] -> Int
countOverlap config sdr = sum . map (choose config sdr) 

-- |Chooses if a column is active or not based on
-- the connection strength to the input space and activation threshold
choose :: HTMConfig -> SDR -> FeedForwardSynapse -> Int
choose config sdr syn = 
  if syn^.conStr >= config^.pConthresh && syn^.ind `elem` sdr 
    then 1 
    else 0


--    Inhibition
-----------------------

-- | Inhibit columns that do not have an overlap score among the k highest
updateInhibition :: HTMConfig -> [Column] -> [Column]
updateInhibition config columns = zipWith (curry (maybeActivateColumn config columns)) columns [1..]

-- |A Column is activated if the it is one the k columns with the  highest activaiton function.
maybeActivateColumn :: HTMConfig -> [Column] -> (Column,Int) -> Column
maybeActivateColumn conH cols (col,ind)=
  maybeActivate col . kmaxOverlap (conH^.colActLev) $ neighbors (col^.inhibRad) ind cols

-- |Returns the neighbors of column within a radius. The radius is clipped if the column is at the edge of the region. -- TODO perhaps use a rotational way?
neighbors :: Int -> Int -> [Column] -> [Column]
neighbors inhibRad colIndex = take inhibRad . drop (colIndex - (inhibRad `div` 2)) --TODO double check

-- |Returns k columns with the highest overlapscore from a list of columns.
kmaxOverlap :: Int -> [Column] -> [Column]
kmaxOverlap k cols = take k $ sortOn _overlap cols -- TODO double check

-- |Activate a column if it is in a list of columns, else Inactivate it.
maybeActivate :: Column -> [Column] -> Column
maybeActivate col cols = 
  if col `elem` cols 
    then col & columnState .~ ActiveColumn & adc %~ on 
    else col & columnState .~ InActiveColumn & adc %~ off


--    LEARN
-----------------------

-- |Update the connection strength of synapses within a region
learn :: HTMConfig -> SDR -> [Column] -> [Column]
learn conH sdr cols = map (updateBoost conH cols . learnCol conH sdr) cols -- TODO might be a problem if the connection strength of currentStep and prevStep are different! Maybe separate the synapses from the columns

learnCol :: HTMConfig -> SDR -> Column -> Column
learnCol conH sdr col = 
  if _columnState col == ActiveColumn 
    then col & inputField %~ map (activateSynapse conH sdr)
    else col

activateSynapse :: HTMConfig -> SDR ->  FeedForwardSynapse -> FeedForwardSynapse
activateSynapse conH sdr syn =  
  if _ind syn `elem` sdr  -- if this synpase is active.
    then syn & conStr %~ min 1 . (+  conH^.proxSynConInc)
    else syn & conStr %~ max 0 . subtract ( conH^.proxSynConDec)



updatePermanence :: Region -> Region
updatePermanence r = r -- TODO

updateBoost :: HTMConfig -> [Column] -> Column -> Column
updateBoost conH cols = updateBoostFactor conH . checkAvgOverlap conH

checkAvgOverlap :: HTMConfig -> Column -> Column
checkAvgOverlap conH col = 
  if averagePercent (col^.odc) < _mop conH 
    then col & (inputField . traverse . conStr) %~ (min 1 . (+ 0.1 {- arbitrary value, should be a parameter.-} *  conH^.pConthresh))
    else col

updateBoostFactor :: HTMConfig -> Column -> Column
updateBoostFactor conH col = col & overlap %~ round . (boostFactor *) . fromIntegral
  where 
    boostFactor = exp $ - conH^.boostStrength * (shift - center)
    shift = average $  col^.adc
    center  =  conH^.targetDensity * fromIntegral (col^. (adc . window))



{--------------------------------------------------------------
                           Temporal Pooler
---------------------------------------------------------------}

--TODO to maintain fixed sparcity, we must grow synapses to active cells in prev

-- TODO applie a spacialPooler on a region
temporalPooler :: HTMConfig -> RegionConfig -> Region -> IO Region
temporalPooler conH conR r =  (addContext conH conR . computeMatchingStrength conH) r <&> switch . predict conH


---------------------------------------------------------------
--                  Compute Matching Strength
---------------------------------------------------------------
computeMatchingStrength :: HTMConfig -> Region -> Region
computeMatchingStrength conH r = r & currentStep . traverse . cells . traverse . dendrites . traverse . traverse %~ setMatchingStrength conH (r^.previousStep)

setMatchingStrength :: HTMConfig -> [Column] -> Segment -> Segment
setMatchingStrength conH prev seg = seg & matchingStrength .~ sumOn' (boolToInt . synapseIsActive conH prev ) (seg^.synapses)


---------------------------------------------------------------
--                          Add Context
---------------------------------------------------------------

addContext :: HTMConfig -> RegionConfig -> Region -> IO Region
addContext conH conR r = do 
  a <- activateColumns conH conR (r^.previousStep) $ zip (r^.currentStep) (r^.previousStep)
  return $ r & currentStep .~ a


activateColumns :: HTMConfig -> RegionConfig ->  [Column] -> [(Column,Column)] -> IO [Column]
activateColumns conH conR prev = mapM (activateColumn conH conR prev)

activateColumn :: HTMConfig -> RegionConfig -> [Column] -> (Column, Column) -> IO Column -- TODO clean up, you are passing in prev twice
activateColumn conH conR prev (c,p) = 
    case c^.columnState of 
      ActiveColumn -> do
        activeCells <- activateCells conH conR prev (p^.cells)
        return $ c & cells .~ activeCells
      InActiveColumn -> return $ p & (cells . traverse) %~ punishPredictedCell conH prev

activateCells :: HTMConfig -> RegionConfig -> [Column] -> [Cell] -> IO [Cell]
activateCells conH conR prev cells = 
  if containsPredictiveCell cells 
    then activatePredictedCells conH conR prev cells -- just the predicted cells
    else burst conH conR prev cells -- all cells

containsPredictiveCell :: [Cell] -> Bool
containsPredictiveCell [] = False
containsPredictiveCell (x:xs)
  |  x^.cellState == PredictiveCell = True
  |  x^.cellState == ActivePredictiveCell = True
  | otherwise = containsPredictiveCell xs 

activatePredictedCells :: HTMConfig -> RegionConfig -> [Column] -> [Cell] -> IO [Cell]
activatePredictedCells conH conR prev = mapM (activatePredictedCell conH conR prev)

activatePredictedCell :: HTMConfig -> RegionConfig -> [Column] -> Cell -> IO Cell
activatePredictedCell conH conR prev cell 
    | cell^.cellState == PredictiveCell = do 
        ndend <- maintainSparcityPerDendrite conH conR prev cell $ learnActiveSegments conH prev (cell^.dendrites)
        return cell {_isWinner = True, _cellState = ActiveCell, _dendrites = ndend}
    | otherwise = return cell {_cellState = InActiveCell}
     


-- | Grow new synpases (nrOfSynapsesPerSegment - matching synapses) on each active Segment.
maintainSparcityPerDendrite :: HTMConfig -> RegionConfig -> [Column] -> Cell -> [Dendrite] -> IO [Dendrite]
maintainSparcityPerDendrite conH conR prev cell den =  sequence $ den & traverse %~ \x -> sequence $ x & traverse %~ maintainSparcityPerSegment conH conR prev cell -- TODO extract Segment and return an IO Dendrite

maintainSparcityPerSegment :: HTMConfig -> RegionConfig -> [Column] -> Cell -> Segment -> IO Segment
maintainSparcityPerSegment conH conR prev cell seg = 
  if seg^.matchingStrength >= conH^.activationThreshold 
  then do
    let prevWinnerCells = filter (not . connectedToSeg seg) $ collectCells (^.isWinner) prev
    let nrOfNewSynapses = (conR^.nrOfSynapsesPerSegment) - (seg^.matchingStrength)
    syns <- growSynapses conR prevWinnerCells cell nrOfNewSynapses
    let newSeg = seg & synapses %~ (++ syns)
    return newSeg
  else return seg

connectedToSeg :: Segment -> Cell -> Bool
connectedToSeg seg cell = foldl (\b x -> b || (x^.destination) /=  (cell^.cellId)) False (seg^.synapses)



burst :: HTMConfig -> RegionConfig -> [Column] -> [Cell] -> IO [Cell]
burst conH conR prev cells = do
  -- Activate and learn on all cells
  let currentCells = map (\x-> x & cellState .~ ActiveCell & dendrites %~ if conH^.learningEnabled then learnActiveSegments conH prev else id ) cells 
   -- Find the bestMatchingSegment
  let (c, (d, (s, m))) = findBestMatching conH prev currentCells 
  -- Find the winnnerCells
  let prevWinnerCells = collectCells (^.isWinner) prev

  if m >= (conH^.matchingThreshold) 
    -- grow synapses on the bestmatchingsegment
    then do
      let newWinnerCell = currentCells !! c
      -- Find winnercells from the previous iteration that are not connected to this winnercells
      let unConnectedPrevWinnerCells = filter (not . connectedTo newWinnerCell) prevWinnerCells
      -- grow synapses from the newWinnerCell to the winnercells from the previous iteration
      let nrOfNewSynapses = conR^.nrOfSynapsesPerSegment - m
      newSynapses <- growSynapses conR unConnectedPrevWinnerCells newWinnerCell nrOfNewSynapses
      -- Append the new synapses to bestMatchingSegment
      let newWinnerCell = newWinnerCell & dendrites . element d . element s . synapses %~ (++ newSynapses)
      -- Mark winnerCell as the winner.
      let newWinnerCell = newWinnerCell & isWinner .~ True
      
      return $ currentCells & element c .~ newWinnerCell
    -- grow synapses on a new segment on the least used Cell
    else do 
      -- Find the least used cell
      let lc = leastUsedCell currentCells 
      -- Add a new segment on the least used cell
      let newWinnerCell = currentCells !! lc
      -- Find winnercells from the previous iteration that are not connected to this winnercells
      let unConnectedPrevWinnerCells = filter (not . connectedTo newWinnerCell) prevWinnerCells
      -- grow synapses from the newWinnerCell to the winnercells from the previous iteration
      let nrOfNewSynapses = conR^.nrOfSynapsesPerSegment
      newSynapses <- growSynapses conR unConnectedPrevWinnerCells newWinnerCell nrOfNewSynapses
      -- Add a segment with these new synapses to the winnerCell
      let newWinnerCell = addSegment newWinnerCell newSynapses-- TODO
      -- Mark winnerCell as the winner.
      let newWinnerCell = newWinnerCell & isWinner .~ True

      return $ currentCells & element lc .~ newWinnerCell


-- |Prepends a new segment to the first dendrite in this cell.
addSegment :: Cell -> [Synapse] -> Cell 
addSegment cell syns = cell & dendrites %~ (\dend -> ( Segment{_segmentState = InActiveSegment, _synapses = syns, _matchingStrength= length syns} : head dend) : tail dend)

--  Grow new synapses. TODO what happens if there are not enough winnercells?
growSynapses :: RegionConfig -> [Cell] -> Cell -> Int -> IO [Synapse]
growSynapses conR selectedCells cell nrSynapses = do
  gen <- newStdGen
  let shuffledCells = shuffle' selectedCells (length selectedCells) gen
  let selectedWinnerCells = take nrSynapses shuffledCells
  return [newSynapse conR cell wCell | wCell <- selectedWinnerCells]


newSynapse :: RegionConfig -> Cell  -> Cell -> Synapse
newSynapse conR source dest = Synapse{
    _source = source
    , _destination = dest^.cellId
    , _connectionStrength = conR^.initConnectionStrength --TODO should be a distribution around a center
  }


collectCells :: (Cell -> Bool) -> [Column] -> [Cell]
collectCells _ [] = []
collectCells f (x:xs) = filter f (x^.cells) ++ collectCells f xs

connectedTo :: Cell -> Cell -> Bool
connectedTo from to = foldl (foldDend from) False (to^.dendrites)

foldDend :: Cell -> Bool -> Dendrite -> Bool
foldDend cell = foldl (foldSeg cell)

foldSeg :: Cell -> Bool -> Segment -> Bool
foldSeg cell b seg = foldl (checkConnection cell) b (seg^.synapses)

checkConnection :: Cell -> Bool -> Synapse -> Bool
checkConnection cell b syn = b || (syn^.destination) == (cell^.cellId)




-- find the best matching segment
findBestMatching :: HTMConfig -> [Column] -> [Cell] -> (CellIndex, (DendriteIndex, (SegmentIndex, Int)))
findBestMatching conH prev = findOne bestCell (getBestDenSeg conH prev)

getBestDenSeg :: HTMConfig -> [Column] -> Cell -> (DendriteIndex, (SegmentIndex, Int))
getBestDenSeg conH prev cell =  findOne bestDen (getBestDend conH prev) (cell^.dendrites)

getBestDend :: HTMConfig -> [Column] -> Dendrite -> (SegmentIndex, Int)
getBestDend conH prev = findOne bestSeg (getMatchingStrength conH prev)

bestCell :: (CellIndex, (DendriteIndex, (SegmentIndex, Int))) -> (CellIndex, (DendriteIndex, (SegmentIndex, Int))) -> (CellIndex, (DendriteIndex, (SegmentIndex, Int)))
bestCell = largest (snd . snd . snd)

bestDen :: (DendriteIndex, (SegmentIndex, Int)) -> (DendriteIndex, (SegmentIndex, Int)) -> (DendriteIndex, (SegmentIndex, Int))
bestDen = largest (snd . snd)

bestSeg :: (SegmentIndex, Int) -> (SegmentIndex, Int) -> (SegmentIndex, Int)
bestSeg = largest snd 

-- Simplification

-- Assumes there is atleast one element in ls. TODO use a Maybe Monad
findOne :: ((Int,a) -> (Int,a) -> (Int,a)) -> (b -> a) -> [b] -> (Int,a)
findOne f wrap ls = let (x:xs) = zip [0..] [wrap e| e <- ls] 
  in foldr f x xs

largest :: (a -> Int) -> a -> a -> a
largest f x y = if f x > f y then x else y

-- find the least used cell
leastUsedCell :: [Cell] -> CellIndex
leastUsedCell = fst . findOne leastUsed countSegments

leastUsed :: (CellIndex, Int) -> (CellIndex, Int) -> (CellIndex, Int)
leastUsed = largest (negate . snd)

countSegments :: Cell -> Int
countSegments cell =  sum [length den | den <- cell^.dendrites]


getMatchingStrength :: HTMConfig -> [Column] -> Segment -> Int
getMatchingStrength conH prev seg = sumOn' (boolToInt . synapseIsActive conH prev ) (seg^.synapses) 

learnActiveSegments :: HTMConfig -> [Column] -> [Dendrite] -> [Dendrite]
learnActiveSegments conH prev = map (map (learnSynapses conH prev))

learnSynapses :: HTMConfig -> [Column] -> Segment -> Segment
learnSynapses conH prev seg = 
  if seg^.segmentState == ActiveSegment 
    then seg & synapses %~ map (learnSynapse conH prev)
    else seg 

learnSynapse :: HTMConfig -> [Column] -> Synapse -> Synapse
learnSynapse conH prev syn = syn{
  _connectionStrength = 
    if  preCellState == ActiveCell || preCellState == ActivePredictiveCell  -- if this synpase is connected to an active cell.
      then  syn^.connectionStrength + conH^.permanenceIncrement
      else  syn^.connectionStrength - conH^.permanenceDecrement
} where 
  preCellState = getCell (syn^.destination) prev ^. cellState
  
punishPredictedCell :: HTMConfig -> [Column] -> Cell -> Cell
punishPredictedCell conH prev cell = 
  if _cellState cell == PredictiveCell
    then cell & dendrites . traverse . traverse %~ punishSegment conH prev
    else cell

punishSegment :: HTMConfig -> [Column] -> Segment -> Segment
punishSegment conH prev seg = if seg^.matchingStrength >= conH^.matchingThreshold
  then seg & synapses . traverse %~ punishSynapse conH prev
  else seg

punishSynapse :: HTMConfig -> [Column] -> Synapse -> Synapse
punishSynapse conH prev syn = if ActiveCell == getCell (syn^.destination) prev ^. cellState
  then syn & connectionStrength -~ (conH^.predictedDecrement)
  else syn


--------------------------------------------------------------
--                           Predict
---------------------------------------------------------------

predict :: HTMConfig -> Region -> Region
predict conH r = r{
  _currentStep = map (predictColumn conH (r^.previousStep)) (r^.currentStep)
}

predictColumn :: HTMConfig -> [Column] -> Column -> Column
predictColumn conH prev col = col{
  _cells = map (maybePredict conH prev) $ col^.cells
}

-- TODO also add the segment state and segment activation size (nr of active synapses).
maybePredict :: HTMConfig -> [Column] -> Cell -> Cell
maybePredict conH prev cell = 
  cell & cellState %~ (\x -> if predicted &&  x == ActiveCell then ActivePredictiveCell else PredictiveCell)
  where 
    predicted = isPredicted conH prev cell

isPredicted :: HTMConfig -> [Column] -> Cell -> Bool
isPredicted conH prev c = dendriteIsPredicted conH prev $ c^.dendrites


-- TODO consider foldl
dendriteIsPredicted :: HTMConfig -> [Column]-> [Dendrite] -> Bool
dendriteIsPredicted _ _ [] = False 
dendriteIsPredicted conH prev (x:xs) = segmentIsPredicted conH prev x || dendriteIsPredicted conH prev xs

segmentIsPredicted :: HTMConfig -> [Column] -> [Segment] -> Bool
segmentIsPredicted _ _ [] = False
segmentIsPredicted conH prev (x:xs) = synapceIsPredicted conH prev x || segmentIsPredicted conH prev xs

synapceIsPredicted :: HTMConfig -> [Column] -> Segment -> Bool
synapceIsPredicted conH prev seg = getMatchingStrength conH prev seg > conH^.activationThreshold

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

synapseIsActive :: HTMConfig -> [Column] -> Synapse  -> Bool
synapseIsActive conH prev syn  = 
   syn^.connectionStrength >  conH^.connectedPermenance
  && getCell (syn^.destination) prev ^. cellState == ActiveCell


--------------------------------------------------------------
--                         Switch Step
---------------------------------------------------------------

switch :: Region -> Region
switch r = r{_currentStep = r^.previousStep, _previousStep = r^.currentStep}


--TODO
-- switch the steps at each step.

-- add a step value in cell ID (Why?)
-- change the step for the current value.
-- add the pointer of the step value of ID when initCells.
-- See if there is an abstraction that does not need a duplicaiton of the a whole region for each step
-- use the ActivePredictiveCell state of a cell in learning
-- avoid code duplicate


-- Represent input within the previous context
  -- For each active column -> activate predictive cells | activate all cells (burst, if no cell is predictive)
-- Predict next state
  -- For cells with n nr of Active Dentrite -> Predict state
  -- Update PermenanceValue between



-- TODO
-- Clean up and improve Code
-- Add a function that creates a segment
-- Implement temporal pooler
-- TODO perhaps use a rotational representation?

