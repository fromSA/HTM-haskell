-- |
-- Module      : HTM2
-- Description : The Hierarchical temporal memory algorithm, version 2
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
module SRC.HTM.HTM2
  ( spatialPooler,
    temporalPooler,
    apply
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
    (?~),
    makeLenses
  )
import Control.Monad ()
import Data.List.Extra ( maximumBy, minimumBy, sortOn, sumOn' ) 
import Debug.Trace (traceShow, traceShowId)
-- TODO remove
import GHC.Natural (Natural, intToNatural, naturalToInt)
import SRC.CommonDataTypes (Index', BitIndex)
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
import GHC.Generics (Generic)
import Miso (cols_)
import SRC.HTM.Helper
import Data.Ord (comparing)


apply initP initR otherRs = do
    let context = GateInputRegions initR otherRs -- ned previous region and distal
    --let winnerCells = WinnerCells $ collectCells (^. isWinner) (initR ^. cols) -- the winner cells from previous step
    --postR <- temporalPooler initP context $ spatialPooler initP initR 
    --apply initP postR
    temporalPooler initP context $ spatialPooler initP initR 
    

    -- What about regions that are not just duplicates?
    -- We have that a region has only current Cols now. So we can pass a 

-- -------------------------------------------------------------
--                           UPDATE
-- -------------------------------------------------------------

{--------------------------------------------------------------
                           SpatialPooler
---------------------------------------------------------------}


-- $Spatial pooler

-- | The spatial pooler, used to spatially encode an input SDR on a Region.
spatialPooler :: Package -> Region2 -> Region2
spatialPooler p r = r & cols %~ (learn p . updateInhibition p . updateOverlap p . clearColumnState) -- the new region
                   

learn, updateInhibition, updateOverlap :: Package -> [Column] -> [Column]

computeOverlap, learnCol, updateBoost, updateBoostFactor, checkAvgOverlap :: Package -> Column -> Column

clearColumnState :: [Column] -> [Column]
clearColumnState = map (\x -> x & columnState .~ InActiveColumn)

---------------------
-- COMPUTE OVERLAP --
---------------------

-- | Updates the overlap score of all columns.
updateOverlap p = map (computeOverlap p)

-- | Updates the overlap score of a column, also boosts the score 
-- based of the boostfactor computed from mop at previous iteration
computeOverlap p c =
  if overlapScore < p ^. conH . spatialConfig . overlapThreshold
    then -- boost the overlap score, based on the '_boost' factor at the previous iteration.
      c & overlap .~ floor (c ^. boost * fromIntegral overlapScore)
        & odc %~ on
    else
      c & overlap .~ overlapScore
        & odc %~ off
  where
    -- Counts the how many synapses in the inputfield of a column are active
    overlapScore = intToNatural $ sum $ map (fromEnum . isColumnActive p) (c ^. inputField)

-- | Choose if a column is active or not based on 
-- if the connection strength of the Synapse to the input space is larger 
-- than the activation threshold
isColumnActive :: Package -> FeedForwardSynapse -> Bool
isColumnActive p syn = syn ^. conStr >= p ^. conH . spatialConfig . pConthresh && syn ^. ind `elem` p ^. value . sdr

---------------------
--   Inhibition    --
---------------------

-- | Inhibit columns that do not have an overlap score among the k highest
updateInhibition p columns = map (maybeActivateColumn p columns) columns

-- | A Column is activated if the it is one the k columns in its neighborhood with the highest activaiton function.
maybeActivateColumn :: Package -> [Column] -> Column -> Column
maybeActivateColumn p cols col =
  if col ^. overlap >= p ^. conH . spatialConfig . overlapThreshold
    then maybeActivate col . kmaxOverlap (p ^. conH . spatialConfig . colActLev) $ neighbors col cols
    else col & columnState .~ InActiveColumn & adc %~ off

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
kmaxOverlap k cols = drop (length l - k') l  -- TODO test
  where
    k' = naturalToInt k
    l = sortOn _overlap cols

-- | Activate a column if it is in a list of columns, else Inactivate it.
maybeActivate :: Column -> [Column] -> Column
maybeActivate col cols
  | col `elem` cols  = col & columnState .~ ActiveColumn & adc %~ on
  | otherwise = col & columnState .~ InActiveColumn & adc %~ off

---------------------
--      LEARN      --
---------------------

-- | Update the connection strength of synapses within a region and update '_boost' of all columns.
-- TODO might be a problem if the connection strength of currentStep and prevStep are different! Maybe separate the synapses from the columns. 
    -- With the new design, this is probably not a problem.
learn p = map (updateBoost p . learnCol p) 

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
temporalPooler :: Package -> GateInputRegions -> Region2 -> IO Region2
temporalPooler p context r = predict p context <$> addContext p context r

--------------------------------------------------------------
--                           ADD CONTEXT
--------------------------------------------------------------

-- | Put a region within the context of the previous step.
addContext :: Package -> GateInputRegions -> Region2 -> IO Region2
addContext p context currR = do
    let getWinners reg = collectCells (^. isWinner) (reg ^. cols)  -- winnerCell is a cell that is active = predicted right or (selected from bursted cells). Used to grow new synapses to it
    let winnerCellsPerReg = WinnerCells 
                                (context ^. proximalRegs & getWinners)
                                (context ^. distalRegs & map getWinners)
    let prevCols = context ^. proximalRegs . cols
    let currCols = currR ^. cols
    a <- activateColumns p winnerCellsPerReg context $ zip currCols prevCols -- maybe we should add other Regions here too?
    return $ currR & cols .~ a

-- | Update cells in columns.
activateColumns :: Package -> WinnerCells -> GateInputRegions -> [(Column, Column)] -> IO [Column]
activateColumns p pwc context = mapM (activateColumn p pwc context)

-- | For active columns, either activate predicted cells or burst.
-- For inactive columns, punish predicted cells.
activateColumn :: Package -> WinnerCells -> GateInputRegions -> (Column, Column) -> IO Column
activateColumn p pwc context (c, pr) =
  case c ^. columnState of
    ActiveColumn -> do
      activeCells <- activateCells p pwc context (pr ^. cells)
      return $ c & cells .~ activeCells
    InActiveColumn -> do
      return $ pr & (cells . traverse) %~ punishPredictedCell p context

-- | Given a list of cells, either activate predicted cells or burst the cells,
-- depending on if one of the cells is predicted.
activateCells :: Package -> WinnerCells -> GateInputRegions -> [Cell] -> IO [Cell]
activateCells p pwc context cells =
  if containsPredictiveCell cells
    then activatePredictedCells p pwc context cells -- just the predicted cells
    else burst p pwc context cells -- all cells

-- | Check if one of cells are predicted.
containsPredictiveCell :: [Cell] -> Bool
containsPredictiveCell = foldl (\b x -> b || x ^. cellState == PredictiveCell || x ^. cellState == ActivePredictiveCell) False




----------------------------------------------------
--              Active Predicted Cells           ---        
----------------------------------------------------



-- | Given a list of cells, activate those that are perdicted.
activatePredictedCells :: Package -> WinnerCells -> GateInputRegions -> [Cell] -> IO [Cell]
activatePredictedCells p pwc context = mapM (activatePredictedCell p context pwc)

-- | If a cell is predicted then activate the cell, learn on active segments and
-- maintainsparcity by growing new synapses if enough are not active.
activatePredictedCell :: Package -> GateInputRegions -> WinnerCells  -> Cell -> IO Cell
activatePredictedCell p context pwc cell
  | cell ^. cellState == PredictiveCell || cell ^. cellState == ActivePredictiveCell = do
    let proxCols = context ^. proximalRegs . cols
    let learned =
          if p ^. conH . temporalConfig . learningEnabled
            then cell ^. dendrites2 & learnActiveSegmentsPerRegion p context 
            else cell ^. dendrites2 --update to dendrites2
    dends <- maintainSparcityToContext p context pwc cell learned
    return cell {_isWinner = True, _cellState = ActiveCell, _dendrites2 = dends}
  | otherwise = return cell {_isWinner = False, _cellState = InActiveCell}

-- | Grow new synpases (nrOfSynapsesPerSegment - matching synapses) on each active Segment.
maintainSparcityToContext :: Package -> GateInputRegions -> WinnerCells -> Cell -> Dendrites -> IO Dendrites 
maintainSparcityToContext p context pwc cell den = do
    proxD <- mapM (maintainSparcityPerSegment p (pwc^.prox) cell) (den ^. proximal)
    distD <- mapM (\(cells,dens) -> mapM (maintainSparcityPerSegment p cells cell) dens) (zip (pwc^.dist) (den^.distal))
    return $ Dendrites proxD distD

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


----------------------------------------------------
--                      Burst                    ---        
----------------------------------------------------


-- | Activate all cells, learn on active segments and select a winner cell.
-- If there is a best matching segment = @Sm@ then select its assosiated cell as the winner cell,
-- else choose the least used cell as the winner cells and grow a new segment = @Sn@ on it.
-- Then on the winner cell, maintain the sparcity on @Sm@ or @Sn@, i.e. 'maintainSparcity'.
-- Bursting a column implies that the column is about to learn a new sequence of input.
burst :: Package -> WinnerCells -> GateInputRegions -> [Cell] -> IO [Cell]
burst p pwc context cells = do
    -- handle proximal and distanl regions

  -- Activate and learn on all cells
  let currentCells = cells & map (\x -> x   & cellState .~ ActiveCell 
                                            & dendrites2 %~ if p ^. conH . temporalConfig . learningEnabled 
                                                    then learnActiveSegmentsPerRegion p context 
                                                    else id) 
                                                    

  -- Find the bestMatchingSegment
  let Location (Just c) (Just d) (Just s) (Just matchVal) = findBestMatching currentCells 

  -- WinnerCell <- bestMatchingSegment
  -- WinnerCell <- leastUsedCell (the cell with the dendrite that has the fewest segments)
        --  If only proximal dendrite is looked at
            -- WinnerCell <- same as today
        -- Else if both proximal and distal dendrites are looked at
            -- WinnerCell <- bestMacthingSegment from either Proximal or Distal dendrites
            -- WinnerCell <- cell with fewest segment on either proximal and distal dendrites?
        -- Only one winnercell is selected. 
 
  -- Find the winnnerCells
  if matchVal >= (p ^. conH . temporalConfig . learningThreshold)
    then updateBestMatchingSegment p pwc context c d s currentCells
    else updateLeastUsedCell p pwc context currentCells

-- | Grow synapses on the bestmatchingsegment and mark the corresponding cell as the winner.
updateBestMatchingSegment :: Package -> WinnerCells -> GateInputRegions -> Int -> DendriteType -> Int -> [Cell] -> IO [Cell]
updateBestMatchingSegment p pwc context c d s cells = do

  -- Grow synapses on the bestmatchingsegment
  let winnerCell = cells !! c
  winnerCellUpdated <- case d of
        Proximal -> do
            let bestMacthingSeg = (winnerCell ^. dendrites2 . proximal) !! s
            -- Find winnercells from the previous iteration that are not connected to this winnercells
            newSynapses <- maintainSparcity p winnerCell (pwc^.prox) bestMacthingSeg

            -- Append the new synapses to bestMatchingSegment
            let winnerCell1 = winnerCell & dendrites2 . proximal . element s . synapses %~ (++ newSynapses)

            -- Update the matchingStrength on this new cell
            return $ winnerCell1 & dendrites2 . proximal . element s . matchingStrength +~ intToNatural (length newSynapses)

        Distal dv -> do
            let bestMacthingSeg = (winnerCell ^. dendrites2 . distal) !! dv !! s
  
            -- Find winnercells from the previous iteration that are not connected to this winnercells
            newSynapses <- maintainSparcity p winnerCell ((pwc^. dist) !!  dv) bestMacthingSeg -- TODO There might be a problem here!!!!!!

            -- Append the new synapses to bestMatchingSegment
            let winnerCell1 = winnerCell & dendrites2 . distal . element dv . element s . synapses %~ (++ newSynapses)

            -- Update the matchingStrength on this new cell
            return $ winnerCell1 & dendrites2 . distal . element dv . element s . matchingStrength +~ intToNatural (length newSynapses)

  -- Mark winnerCell as the winner.
  return $ cells & element c .~ (winnerCellUpdated & isWinner .~ True)


-- | Grow synapses on a new segment on the least used Cell, and mark the corresponding cell as the winner.
updateLeastUsedCell :: Package -> WinnerCells -> GateInputRegions -> [Cell] -> IO [Cell]
updateLeastUsedCell p pwc context cells = do
  -- Find the least used cell
  let Location (Just lc) (Just d) _ _ = leastUsedCell cells -- get the cell, and the dendrite with fewest segment

  let newWinnerCell = cells !! lc

  -- Add a new segment on the least used cell
  let newSegment = growSegment
  
  winnerCell1 <- 
        case d of 
            Proximal -> do 
                    newSynapses <- maintainSparcity p newWinnerCell (pwc^.prox) newSegment

                    let newSegment2 = newSegment
                                        & synapses .~ newSynapses
                                        & matchingStrength .~ intToNatural (length newSynapses)

                    -- Add the new segment with these new synapses to the winnerCell. prepend a new segment to the first dendrite in this cell.
                    return $ newWinnerCell & dendrites %~ \xs -> (newSegment2 : head xs) : tail xs
            Distal dv -> do
                    newSynapses <- maintainSparcity p newWinnerCell ((pwc^.dist)!!dv) newSegment

                    let newSegment2 = newSegment
                                        & synapses .~ newSynapses
                                        & matchingStrength .~ intToNatural (length newSynapses)


                    -- Add the new segment with these new synapses to the winnerCell. prepend a new segment to the first dendrite in this cell.
                    return $ newWinnerCell & dendrites %~ \xs -> (newSegment2 : head xs) : tail xs



  -- Mark winnerCell as the winner.
  return $ cells & element lc .~ (winnerCell1 & isWinner .~ True)



----------------------------------------------------
--              Maintain Sparcity               ---        
----------------------------------------------------


maintainSparcity :: Package -> Cell -> [Cell] -> Segment -> IO [Synapse]
maintainSparcity p cell pwc seg = do
  -- Find winnercells from the previous iteration that are not connected to this winnercells
  --let WinnerCells pwcs = pwc
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




----------------------------------------------------
--     Get path to Best Matching Segment         ---        
----------------------------------------------------


-- | Finds one element from list of items that meets a condition f after the items are transformed by wrap.
-- Assumes there is atleast one element in ls. 
findOne2 :: (Location BitIndex -> Location BitIndex -> Ordering) -> (Int -> Location BitIndex -> Location BitIndex ) -> (a -> Location BitIndex) -> [a] -> Location BitIndex
findOne2 comparator merger wrapper items =
  let 
    -- parent is a location that contains information about the parent, a cell, dendrite, segment or something.
     -- the comparetor compares two locations and return the location with the best value, if there is any
     -- wrapper takes the parent and the current item and creates a new location with the current item included in the location path.
     -- item is the current item, either cell, dendrite, segment or matchstrength.
    xs = zipWith merger [0..] [wrapper e | e <- items]
   --in foldl comparator x xs
   in maximumBy comparator xs

-- | Find the best matching segment from a collection of Cells
findBestMatching :: [Cell] -> Location BitIndex
findBestMatching cells = let    wrapper = getBestCell -- the next step to call unless it is the base = segment
                                merger currentI location = location & cellIndex ?~ currentI
                            in  findOne2 locationComparator merger wrapper cells


-- | Find the best matching segment from a Cell
getBestCell :: Cell -> Location BitIndex
getBestCell cell = let  mergerProx location = location & dendriteType ?~ Proximal
                        bestProx = mergerProx $ getBestDen (cell^. dendrites2 . proximal) -- list of segments
                        
                        wrapperDest = getBestDen
                        mergerDest currentI location = location & dendriteType ?~ Distal currentI
                        bestDist = findOne2 locationComparator mergerDest wrapperDest (cell^. dendrites2 . distal) -- list of dendrites

                    in  maximumBy locationComparator [bestProx,bestDist]
    
    
   -- findOne bestDen getBestDend2 (cell ^. dendrites)

-- | Find the best matching segment from a Dendrite
getBestDen :: Dendrite -> Location BitIndex
getBestDen den = let    parent = Location Nothing Nothing Nothing Nothing 
                        wrapper seg = parent & bestMatch ?~ seg ^. matchingStrength -- segment -> matching strength
                        merger currentIndex location = location & segmentIndex ?~ currentIndex
                    in  findOne2 locationComparator merger wrapper den



locationComparator ::  Ord a => Location a -> Location a -> Ordering
locationComparator first second = -- compare matching strength if any
    let a = first ^. bestMatch
        b = second ^. bestMatch
    in case (a,b) of
        (Just firstV, Just secondV) -> if firstV > secondV then GT else LT
        (_, _) -> EQ -- just return the next item.


---------------------------------


-- | Find the least used cell, the one with fewest segments. Incase of a tie, it currently picks the first such cell.
-- TODO In the future, this will be break ties randomly.
leastUsedCell :: [Cell] -> Location Int
leastUsedCell cells = let   locs = map smallestDendrite cells
                            comparator loc1 loc2 = compare (loc1^.bestMatch) (loc2^.bestMatch)
                            merger i loci = loci & cellIndex ?~ i
                        in minimumBy comparator $ zipWith merger [0..] locs
                        

-- | get location of dendrite (proximal or distal) with the fewest number of segments
smallestDendrite :: Cell -> Location Int
smallestDendrite cell = let loc = Location Nothing Nothing Nothing Nothing :: Location Int
                            Dendrites prox dist = cell ^. dendrites2
                            proxLength = loc & dendriteType ?~ Proximal
                                                & bestMatch ?~ length prox  
                            merger i len = loc & dendriteType ?~ Distal i
                                                & bestMatch ?~ len 
                            distLength = minimumBy locationComparator $ zipWith merger [0..] $ map length dist 
                        in minimumBy locationComparator [proxLength, distLength]
    

--- Learn

-- | Update the '_connectionStrength' of each synapse connected to an active cell
--  on all active segments with '_permanenceIncrement' or '_permanenceDecrement'.
learnActiveSegments :: Package -> PreviousColumns -> [Dendrite] -> [Dendrite] -- TODO Handle both proximal and distal 
learnActiveSegments p prev = map (map (learnSynapses p prev))



-- | Update the '_connectionStrength' of each synapse connected to an active cell
--  on an active segments with '_permanenceIncrement' or '_permanenceDecrement'.
learnSynapses :: Package -> PreviousColumns -> Segment -> Segment
learnSynapses p prev seg =
  if seg ^. segmentState == ActiveSegment
    then seg & synapses %~ map (learnSynapse p prev)
    else seg

-- | Update the '_connectionStrength' of each synapse with '_permanenceIncrement'
-- if it is connected to an 'ActiveCell' or 'AtivePredictiveCell'
-- and with '_permanenceDecrement', otherwise.
learnSynapse :: Package -> PreviousColumns -> Synapse -> Synapse
learnSynapse p prev syn =
  if preCellState == ActiveCell || preCellState == ActivePredictiveCell -- if this synpase is connected to an active cell.
    then syn & connectionStrength +~ p ^. conH . temporalConfig . permanenceIncrement
    else syn & connectionStrength -~ p ^. conH . temporalConfig . permanenceDecrement
  where
    PreviousColumns prevCols = prev
    preCellState = getCell (syn ^. source {-changed-}) prevCols ^. cellState

learnActiveSegmentsPerRegion :: Package -> GateInputRegions -> Dendrites -> Dendrites
learnActiveSegmentsPerRegion p context den = 
    let ds = map (PreviousColumns . (^.cols)) $ context ^. distalRegs 
        f prevCols = map (learnSynapses p prevCols)
    in den  & proximal  %~ map (learnSynapses p (PreviousColumns $ context ^. proximalRegs . cols))
            & distal    %~ zipWith f ds



--- Punich

-- | For all synapses belonging to a cell, decrement the '_connectionStrength' with
-- '_predictedDecrement' if the '_cellState' is 'PredictiveCell'
-- and the segment is 'MatchingSegment' or 'ActiveSegment'
-- and the synapses is connected to an active cell.
-- Punishing a synapses will help the region learn from wrong predictions.
punishPredictedCell :: Package -> GateInputRegions -> Cell -> Cell
punishPredictedCell p context cell =
  if cell ^. cellState == PredictiveCell || cell ^. cellState == ActivePredictiveCell
    then cell   & dendrites2 . proximal . traverse %~ punishSegment p (PreviousColumns (context ^. proximalRegs . cols))
                & dendrites2 . distal .~ distal2
                 & cellState .~ InActiveCell
    else cell & cellState .~ InActiveCell
    where prevCols = map (punishSegment p . PreviousColumns . (^.cols)) (context ^. distalRegs)
          func den f =  den & traverse %~ f 
          distal2 = zipWith func (cell ^. dendrites2 . distal) prevCols

-- | For all synapses belonging to a cell, decrement the '_connectionStrength' with
-- '_predictedDecrement' if a segment is 'MatchingSegment' or 'ActiveSegment'
-- and the synapses is connected to an active cell
punishSegment :: Package -> PreviousColumns -> Segment -> Segment
punishSegment p prev seg =
  if seg ^. segmentState == MatchingSegment || seg ^. segmentState == ActiveSegment
    then seg & synapses . traverse %~ punishSynapse p prev
    else seg

-- | For all synapses belonging to a cell, decrement the '_connectionStrength' with
-- '_predictedDecrement' if a synapse is connected to an active cell
punishSynapse :: Package -> PreviousColumns -> Synapse -> Synapse
punishSynapse p prev syn =
    let PreviousColumns prevCols = prev in
    if ActiveCell == getCell (syn ^. source {-changed-}) prevCols ^. cellState 
        then syn & connectionStrength -~ p ^. conH . temporalConfig . predictedDecrement
        else syn

--------------------------------------------------------------
--                           Predict
--------------------------------------------------------------


predict :: Package -> GateInputRegions -> Region2 -> Region2
predict p ors = 
    let predictor = cols %~ map (predictCells p) -- change state to predicted based on segment states
        cleanSynapses = updateAllSegments removeDeadSynapses -- remove the dead synapses
        updateState = updateAllSegments $ updateSegmentState p  -- update the state of the segments based of the computes matching strength
        prxReg = ors^.proximalRegs
        distRegs = ors^.distalRegs
        updateDistalDends = updateDistalDendrites (zipWith (curry (computeMatchingStrengthDen p)) distRegs) -- update all distal dendrites using the region they read from
        updateProximalDens = updateProximalDendrite (\den -> computeMatchingStrengthDen p (prxReg, den))
    in 
        predictor . updateState . updateDistalDends . updateProximalDens . cleanSynapses

type ProximalDendrite = Dendrite 
type DistalDendrites = [Dendrite]

updateProximalDendrite :: (ProximalDendrite -> ProximalDendrite) -> Region2 -> Region2
updateProximalDendrite f r = r & cols . traverse . cells . traverse . dendrites2 . proximal %~ f

updateDistalDendrites :: (DistalDendrites -> DistalDendrites) -> Region2 -> Region2
updateDistalDendrites f r = r & cols . traverse . cells . traverse . dendrites2 . distal %~ f


computeMatchingStrengthDen :: Package -> (Region2, Dendrite) -> Dendrite
computeMatchingStrengthDen p (r, dens) = dens & traverse %~ computeMatchingStrength p r



updateAllSegments :: (Segment -> Segment) -> Region2 -> Region2
updateAllSegments f r = r & cols . traverse . cells . traverse . dendrites . traverse . traverse %~ f

-------------------------------
--   Remove dead synapses    --
-------------------------------
removeDeadSynapses :: Segment -> Segment
removeDeadSynapses seg = seg & synapses %~ filter (\syn -> syn ^. connectionStrength > 0)

-------------------------------
-- Compute Matching Strength --
-------------------------------

newtype ActiveCells = ActiveCells [Cell]

--matchDendriteWithRegions :: [Dendrite] -> [Region] -> [(Dendrite, Region)]

--updateAllDendrites :: Region -> [(Dendrite, Region)] -> [Dendrite] -- A Dendrite gets its ActiveCells from a specific Region
-- get regions cells from the respective Region of a dendrite



computeMatchingStrength :: Package -> Region2 -> Segment -> Segment
computeMatchingStrength p prev seg = seg & matchingStrength .~ sumOn' (intToNatural . fromEnum . synapseIsActive p prev) (seg ^. synapses)

synapseIsActive :: Package -> Region2 -> Synapse -> Bool
synapseIsActive p prev syn =
  syn ^. connectionStrength > p ^. conH . temporalConfig . connectedPermenance
    &&  cellS == ActivePredictiveCell || cellS == ActiveCell
    where 
      cellS = getCell (syn ^. source {-changed-}) (prev ^.cols) ^. cellState

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
predictCells p col = col 
  & cells %~ map (predictCell p) 

-- | Helper function of predictColumn
predictCell :: Package -> Cell -> Cell
predictCell p cell =
    if p ^. (conH. temporalConfig. selfPredict) &&  predicted
      then cell & cellState %~ (\x -> if x == ActiveCell || x == ActivePredictiveCell then ActivePredictiveCell else PredictiveCell)
      else cell & cellState %~ (\x -> if x == ActiveCell || x == ActivePredictiveCell then ActiveCell else InActiveCell)
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


