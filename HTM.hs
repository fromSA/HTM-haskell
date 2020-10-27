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
module HTM (
  -- * Some functions
  spacialPooler, temporalPooler) where

import           System.Random
import           MovingAverage 
import           SDR           (SDR, SDRConfig(..), SDRRange(..), encode, totNrBits)
import           Region
import           Data.List.Extra (intercalate, sortOn, sumOn')
-- -------------------------------------------------------------
--                           CONFIG
-- -------------------------------------------------------------

-- |The configuration parameters for the HTM algorithm.
data HTMConfig = HTMConfig{
  overlapThreshold         :: Int -- ^ The threshold for column activation. If the number of active bits in the inputfield of a column >= this threshold, then the column becomes active.
  , pConthresh             :: Float -- ^ A synapse with connection strength >= this threshold is considered permenantly connected.
  , colActLev              :: Int -- ^ The desired column activity level within inhibition radius i.e. the number of columns in that should be activated within a activaiton radius.
  , proxSynConInc :: Float -- ^The amount to decrease the connection strength of a synapses with for proximal synapses 
  , proxSynConDec :: Float -- ^The amount to decrease the connection strength of a synapses with for proximal synapses 
  , mop :: Float -- ^The minimum percent of active bits in inputField expected to have a overlap with.
  , boostStrength :: Float -- ^The strength of boost value between about 0
  , targetDensity :: Float -- ^The desired percent of active duty cycle within the sliding window.
  , connectedPermenance :: Float -- ^The threshold of permenant synaptic connection. If a synapse has higher connection than this threshold and is connected to an active cell, then it is considered active. 
  , activationThreshold :: Int -- ^The number of synapses per segment that must be active for the segment to be considered active. should be less than nrOfSynapsesPerSegment of RegionConfig
  , predictedDecrement :: Float 
  , permanenceDecrement :: Float -- ^The forgetting strength, detrmines how fast a pattern is forgotten by the region.
  , permanenceIncrement :: Float -- ^The learning strength, determines how fast a pattern is learned by the region
}


-- -------------------------------------------------------------
--                           UPDATE
-- -------------------------------------------------------------


{--------------------------------------------------------------
                           SpatialPooler
---------------------------------------------------------------}


spacialPooler :: HTMConfig -> SDR -> Region -> Region
spacialPooler conH sdr region = region { 
  --currentStep = learn conH sdr . updateInhibition conH $ updateOverlap conH sdr (currentStep region)
  currentStep = learn conH sdr . updateInhibition conH $ updateOverlap conH sdr (currentStep region)
  }

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
computeOverlap conH sdr column = let overlap = countOverlap conH sdr (inputField column) in
  if overlap >= overlapThreshold conH 
  then column{overlap = floor $ fromIntegral overlap  * boost column, odc = on (odc column)}
  else column{overlap = 0 , odc = off (odc column)}


-- |Counts the how many synapses in the inputfield of a column are active 
countOverlap :: HTMConfig -> SDR -> [FeedForwardSynapse] -> Int
countOverlap config sdr = sum . map (choose config sdr) 

-- |Chooses if a column is active or not based on
-- the connection strength to the input space and activation threshold
choose :: HTMConfig -> SDR -> FeedForwardSynapse -> Int
choose config sdr syn 
  | conStr syn >= pConthresh config = if ind syn `elem` sdr then 1 else 0
  | otherwise = 0


--    Inhibition
-----------------------

-- | Inhibit columns that do not have an overlap score among the k highest
updateInhibition :: HTMConfig -> [Column] -> [Column]
updateInhibition config columns = zipWith (curry (maybeActivateColumn config columns)) columns [1..]

-- |A Column is activated if the it is one the k columns with the  highest activaiton function.
maybeActivateColumn :: HTMConfig -> [Column] -> (Column,Int) -> Column
maybeActivateColumn conH cols (col,ind)=
  maybeActivate col . kmaxOverlap (colActLev conH) $ neighbors (inhibRad col) ind cols

-- |Returns the neighbors of column within a radius. The radius is clipped if the column is at the edge of the region. -- TODO perhaps use a rotational way?
neighbors :: Int -> Int -> [Column] -> [Column]
neighbors inhibRad colIndex = take inhibRad . drop (colIndex - (inhibRad `div` 2)) --TODO double check

-- |Returns k columns with the highest overlapscore from a list of columns.
kmaxOverlap :: Int -> [Column] -> [Column]
kmaxOverlap k cols = take k $ sortOn overlap cols -- TODO double check

-- |Activate a column if it is in a list of columns, else Inactivate it.
maybeActivate :: Column -> [Column] -> Column
maybeActivate col cols = 
  if col `elem` cols 
    then col{columnState = ActiveColumn, adc = on (adc col)} 
    else col{columnState = InactiveColumn, adc = off (adc col)}


--    LEARN
-----------------------

-- |Update the connection strength of synapses within a region
learn :: HTMConfig -> SDR -> [Column] -> [Column]
learn conH sdr cols = map (updateBoost conH cols . learnCol conH sdr) cols -- TODO might be a problem if the connection strength of currentStep and prevStep are different! Maybe separate the synapses from the columns

learnCol :: HTMConfig -> SDR -> Column -> Column
learnCol conH sdr col = 
  if columnState col == ActiveColumn 
    then col{inputField = map (activateSynapse conH sdr) (inputField col)}
    else col

activateSynapse :: HTMConfig -> SDR ->  FeedForwardSynapse -> FeedForwardSynapse
activateSynapse conH sdr syn =  
  if ind syn `elem` sdr  -- if this synpase is active.
    then syn {conStr = min 1 (conStr syn + proxSynConInc conH)} 
    else syn {conStr = max 0 (conStr syn - proxSynConDec conH)}



updatePermanence :: Region -> Region
updatePermanence r = r -- TODO

updateBoost :: HTMConfig -> [Column] -> Column -> Column
updateBoost conH cols col = updateBoostFactor conH $ checkAvgOverlap conH col

checkAvgOverlap :: HTMConfig -> Column -> Column
checkAvgOverlap conH col = 
  if averagePercent (odc col) < mop conH 
    then col{
      inputField = map (\x -> x{
        conStr = min 1 (conStr x + 0.1 * pConthresh conH)
        }) (inputField col)
      }
    else col

updateBoostFactor :: HTMConfig -> Column -> Column
updateBoostFactor conH col = col{overlap =  round (boostFactor * fromIntegral (overlap col))}
  where 
    boostFactor = exp $ - boostStrength conH * (shift - center)
    shift = average $ adc col
    center  = targetDensity conH * fromIntegral (_window $ adc col)

{--------------------------------------------------------------
                           Temporal Pooler
---------------------------------------------------------------}

--TODO to maintain fixed sparcity, we must grow synapses to active cells in prev

-- TODO applie a spacialPooler on a region
temporalPooler :: HTMConfig -> Region -> Region
temporalPooler conH = predict conH . addContext conH

--------------------------------------------------------------
--                           Add Context
---------------------------------------------------------------

addContext :: HTMConfig -> Region -> Region
addContext conH r = r{
  currentStep = activateColumns conH (previousStep r) $ zip (currentStep r) (previousStep r) 
}

activateColumns :: HTMConfig ->  [Column] -> [(Column,Column)] -> [Column]
activateColumns conH prev = map (activateColumn conH prev)

activateColumn :: HTMConfig -> [Column] -> (Column, Column) -> Column -- TODO clean up, you are passing in prev twice
activateColumn conH prev (c,p) 
  | columnState c == ActiveColumn = c {cells = activateCells conH prev $ cells p}
  | columnState c == InactiveColumn =  punishPredictedCells conH p

activateCells :: HTMConfig -> [Column] -> [Cell] -> [Cell]
activateCells conH prev cells = 
  if containsPredictiveCell cells 
    then activatePredictedCells cells -- just the predicted cells
    else burst conH prev cells -- all cells

containsPredictiveCell :: [Cell] -> Bool
containsPredictiveCell [] = False
containsPredictiveCell (x:xs)
  | cellState x == PredictiveCell = True
  | cellState x == ActivePredictiveCell = True
  | otherwise = containsPredictiveCell xs 

activatePredictedCells :: [Cell] -> [Cell]
activatePredictedCells (x:xs) = 
  if cellState x == PredictiveCell  -- Also learn on active segments.
    then x{cellState = ActiveCell}: activatePredictedCells xs
    else x: activatePredictedCells xs

burst :: HTMConfig -> [Column] -> [Cell] -> [Cell]
burst conH prev = map (\x-> x{
  dendrites = learnDendrites conH prev $ dendrites x -- TODO also grow new synapses on bestmatching segment or least used segment.
  , cellState = ActiveCell
  }) 

learnDendrites :: HTMConfig -> [Column] -> [Dendrite] -> [Dendrite]
learnDendrites conH prev = map (learnSegmentes conH prev)

learnSegmentes :: HTMConfig -> [Column] -> [Segment] -> [Segment]
learnSegmentes conH prev = map (learnSynapses conH prev)


learnSynapses :: HTMConfig -> [Column] -> Segment -> Segment
learnSynapses conH prev seg = 
  if segmentState seg == ActiveSegment 
    then seg{synapses = map (learnSynapse conH prev) $ synapses seg}
    else seg 

learnSynapse :: HTMConfig -> [Column] -> Synapse -> Synapse
learnSynapse conH prev syn = syn{
  connectionStrength = 
    if cellState preCell == ActiveCell || cellState preCell == ActivePredictiveCell  -- if this synpase is connected to an active cell.
      then connectionStrength syn + permanenceIncrement conH
      else connectionStrength syn - permanenceDecrement conH
} where 
  preColumn = prev !! col_ id
  preCell = cells preColumn !! cell_ id
  id = destination syn
  
punishPredictedCells :: HTMConfig -> Column -> Column
punishPredictedCells conH col = col{
    cells = punishCells conH $ cells col
  }

punishCells :: HTMConfig -> [Cell] -> [Cell]
punishCells conH = map $ punishCell conH
  
punishCell :: HTMConfig -> Cell -> Cell
punishCell conH cell = 
  if cellState cell == PredictiveCell
    then cell{dendrites = punishDendrites $ dendrites cell}
    else cell

punishDendrites :: [Dendrite] -> [Dendrite]
punishDendrites r = r -- TODO
-- predictedDecrement conH

--------------------------------------------------------------
--                           Predict
---------------------------------------------------------------

predict :: HTMConfig -> Region -> Region
predict conH r = r{
  currentStep = map (predictColumn conH (previousStep r)) (currentStep r)
}

predictColumn :: HTMConfig -> [Column] -> Column -> Column
predictColumn conH prev col = col{
  cells = map (maybePredict conH prev) $ cells col 
}
-- TODO also add the segment state and segment activation size (nr of active synapses).
maybePredict :: HTMConfig -> [Column] -> Cell -> Cell
maybePredict conH prev cell = cell{
  cellState = if predicted && cellState cell == ActiveCell then ActivePredictiveCell else PredictiveCell
  } 
  where 
    predicted = isPredicted conH prev cell

isPredicted :: HTMConfig -> [Column] -> Cell -> Bool
isPredicted conH prev c = dendriteIsPredicted conH prev $ dendrites c

dendriteIsPredicted :: HTMConfig -> [Column]-> [Dendrite] -> Bool
dendriteIsPredicted _ _ [] = False 
dendriteIsPredicted conH prev (x:xs) = segmentIsPredicted conH prev x || dendriteIsPredicted conH prev xs

segmentIsPredicted :: HTMConfig -> [Column] -> [Segment] -> Bool
segmentIsPredicted _ _ [] = False
segmentIsPredicted conH prev (x:xs) = synapceIsPredicted conH prev x || segmentIsPredicted conH prev xs

synapceIsPredicted :: HTMConfig -> [Column] -> Segment -> Bool
synapceIsPredicted conH prev seg = sumOn' (boolToInt . synapseIsActive conH prev ) (synapses seg) > activationThreshold conH

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

synapseIsActive :: HTMConfig -> [Column] -> Synapse  -> Bool
synapseIsActive conH prev syn  = 
  connectionStrength syn > connectedPermenance conH 
  && cellState cell  == ActiveCell
  where 
    column = prev !! col_ id
    cell = cells column !! cell_ id
    id = destination syn


--TODO
-- punish predicted cells, 
-- add a step value in cell ID
-- change the step for the current value.
-- add the pointer of the step value of ID when initCells.
-- grow synapses when learning
-- switch the steps at each step.
-- See if there an abstraction that does not need a duplicaiton of the a whole region for each step
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


-- -------------------------------------------------------------
--                           VIEW
-- -------------------------------------------------------------

main = do
  gen <- newStdGen
  let sdrConfig = initSDRConfig
  let regionConfig = initRegionConfig
  let htmConfig = initHTMConfig
  region <- initRegion sdrConfig regionConfig
  let dat = [50,20,25,20]
  compute dat htmConfig sdrConfig region
-- main = print <$> spacialPooler <*> (encode 12 sdrConfig) <*> (initRegion initConfig initSDRConfig)

compute :: [Int] -> HTMConfig -> SDRConfig -> Region -> IO ()
compute [] _ _ region = print region
compute (x:xs) conH conS region = do
  print region
  let encodedSDR = encode x conS
  let region2 = spacialPooler conH encodedSDR region
  compute xs conH conS region2

initSDRConfig :: SDRConfig
initSDRConfig = SDRConfig{
  minVal          = 0
  , maxVal        = 100
  , buckets       = 50
  , bitsPerBucket = 30
  , sdrRange = SDRRange {minIndex  = 0, maxIndex = sum [50{- bucket -}, 30{- bitsPerBucket -}] - 1}
}

initRegionConfig = RegionConfig{
  nrOfColumns = 100
  , nrOfCellsPerColumn = 1
  , maxNrOfInputBits = 3
  , nrOfSynapsesPerSegment = 4
  , mappingType = Random
  , initConnectionStrength = 1.0
  , mvWindow = 3
}


initHTMConfig = HTMConfig{
  overlapThreshold = 0
  , pConthresh = 0.2
  , colActLev = 1
  , proxSynConInc = 0.2
  , proxSynConDec = 0.2
  , mop = 0.2
  , targetDensity = 0.3
  , boostStrength = 0.3
  , connectedPermenance = 0.5
  , activationThreshold = 2 
  , predictedDecrement = 0.1
  , permanenceIncrement = 0.1
  , permanenceDecrement = 0.1
}