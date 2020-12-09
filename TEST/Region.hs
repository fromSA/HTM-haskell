{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module TEST.Region where

import Test.QuickCheck
import SRC.Region.Region
import Control.Lens((^.))
import SRC.Encoder.Numeric
import GHC.Natural ( intToNatural )
import GHC.Generics (Generic)
import SRC.Encoder.Config
import System.Random (mkStdGen, Random(randomR))

---- TO test
--- getters, 
-- getcell, 
-- getRandomCell, 
-- selectRandomindecies, 
-- getRandomBetween
--- inits
-- initRegion
-- initColumns, initSingleColumn
-- initCells
-- initSegment
-- initDendrites
-- initSingleCell
-- initAllDendrites
-- initDendritesPerCell
-- initSynapse
-- newSynapse
-- initFeedForwardSynapse
-- singleFeedForwardSynapse


-------------------------------------
--           Invariants            --
  

-- chin = CHeck INvariance
between :: Ord a => a -> a -> a -> Bool
between mi ma v = mi <= v && v <= ma

allAreValid :: (a -> Bool) -> [a] ->  Bool
allAreValid f = foldl (\b x -> b && f x) True

-- Valid connectionStrength 
--  0<= connecitonStrength <=1  (between)

chinConnectionStrength :: ConnectionStrength -> Bool
chinConnectionStrength = between 0 1 

--- Valid RegionConfig 
--      0 <= nrOfSynpasesPerSegment <= nrOfCellsPerColumn*nrOfColumns (between)
--      initConnectionStrength must be valid connecitonstrength. If it is 0, then the synapse will be removed
--      _mvWindow > 0
--      0 <= _initNrOfFeedForwardSynpases <= Encoder.totNrBits (between)
chinRegionConfig :: EncoderConfig -> RegionConfig -> Bool
chinRegionConfig e c = 
    between 0 (c^.nrOfCellsPerColumn*c^.nrOfColumns) (c^.nrOfSynapsesPerSegment)
    && chinConnectionStrength (c^.initConnectionStrength)
    && (c^.mvWindow)>0
    &&  between 0 (totNrBits e) (c^.initNrOfFeedForwardSynpases)

--- Valid RegionModel

-- Valid CellID:
--  0<= CellId_col <= nrOfColumns (between)
--  0<= CellID_cell <= nrOfCellsPerColumn (between)

chinCellID :: RegionConfig -> CellID -> Bool
chinCellID r c =  
    between 0 (r^.nrOfColumns) (c^.col)
    && between 0 (r^.nrOfCellsPerColumn) (c^.cell)

-- ValidSynapse:
--  Synapse_source must be a valid CellID
--  Synapse_source must be a valid CellID
--  ConnectionStrength must be a valid connectionStrength

chinSynapse :: RegionConfig -> Synapse -> Bool
chinSynapse r s = 
    chinCellID r (s^.source)
    && chinCellID r (s^.destination)
    && chinConnectionStrength (s^.connectionStrength)

-- Valid Segment:
--  0<= Segment_synapses <= valid nrOfSynapses (between)
--  All Segment_synapses must be valid synapses
--  0<= Segment_matchingStrength <= length Segment_synapses <= valid nrOfSynapses  (between)

-- Assuming valid RegionConfig
chinSegment :: RegionConfig -> Segment -> Bool
chinSegment r s = 
    between 0 (r^.nrOfSynapsesPerSegment) (intToNatural (length (s^.synapses)))
    && foldl (\b x -> b && chinSynapse r x) True (s^.synapses)
    && between 0 (intToNatural (length (s^.synapses))) (s^.matchingStrength)
    && between 0 (r^.nrOfSynapsesPerSegment) (s^.matchingStrength)

-- Valid dendrite:
--  all segments are valid

chinDendrite :: RegionConfig -> Dendrite -> Bool
chinDendrite r = allAreValid (chinSegment r)

-- ValidCell:
--  Cell_id is valid CellID
--  dendrites are valid

chinCell :: RegionConfig -> Cell -> Bool
chinCell r c = 
    chinCellID r (c^.cellId)
    &&  allAreValid (chinDendrite r) (c^.dendrites)

-- Valid FeedForwardSynapse
--  0 <= ind <= Encoder.totNrBits (between)
--  conStr is a valid connectionStrength

chinFeedForwardSynapse :: EncoderConfig -> FeedForwardSynapse -> Bool
chinFeedForwardSynapse e s = 
    between 0 (totNrBits e) (s^.ind) 
    && chinConnectionStrength (s^.conStr)

-- Valid Column:
--  0 <= columnId <= RegionConfig.nrOfColumns (between)
--  cells are all valid
--  0<= inputField <= Encoder.totNrBits (between) && all feedforwardsynapses are valid
--  boost >= 0
--  0<= overlap <= Encoder.totNrBits (between) && 0<= overlap <= inputField
--  0<= inhibRad <= Region.nrOfColumns (between) 
--  adc is a valid MovingAverage
--  odc is a valid MovingAverage
--  all cells in a column i, have the same columnId.

chinColumn :: EncoderConfig -> RegionConfig -> Column -> Bool
chinColumn e r c = 
    between 0 (r^.nrOfColumns) (c^.columnId)
    && allAreValid (chinCell r) (c^.cells)
    && between 0 (totNrBits e) (intToNatural (length (c^.inputField)))
    && allAreValid (chinFeedForwardSynapse e) (c^.inputField)
    && (c^.boost) >= 0
    && between 0 (totNrBits e) (c^.overlap)
    && between 0 (intToNatural (length (c^.inputField))) (c^.overlap)
    && snd (foldl (\(b,s) x -> (b, s && b == x^.cellId.col)) (c^.columnId, True) (c^.cells))
    -- && chinMovingAverage (c^.adc)
    -- && chingMovingAverage (c^.odc)


-- Valid Region:
--  currentstep, all columns are valid
--  previousstep, all columns are valid
--  lenth currentStep == length previousStep
--  foreach cell in currentstep, there is a cell in previousstep with the same id; and viseversa.
--  foreach column with id i in currentstep, there is a column in previousstep with the same id; and viseversa.
--  all columns are ordered similarly
chinRegion :: EncoderConfig -> RegionConfig -> Region -> Bool
chinRegion e c r = 
    allAreValid (chinColumn e c) (r^.currentStep)
    && allAreValid (chinColumn e c) (r^.previousStep)
    && length (r^.currentStep) == length (r^.previousStep)
    && foldl (\b x -> b && sameColumnIds x) True (zip (r^.currentStep) (r^.previousStep))
    where
        sameColumnIds (c1, c2) = c1^.columnId == c2^.columnId


-------------------------------------
--           Arbitraries           --

instance Arbitrary EncoderConfig where
    arbitrary = return defualtEncoderConfig

instance Arbitrary RegionConfig where
    arbitrary = return defualtRegionConfig

instance Arbitrary Region where
    arbitrary = do
        int <- arbitrary :: Gen Int
        let stdGen = mkStdGen int
        let (a,stdGen2) = randomR (1,5) stdGen
        let (b,_) = randomR (1,5) stdGen2
        encoderConfig  <-  arbitrary :: Gen EncoderConfig
        regionConfig <- arbitrary :: Gen RegionConfig

        --return =<< initRegion encoderConfig regionConfig

        


-------------------------------------
--           Properties            --

--- getters, 
-- getcell, 
-- getRandomCell, 
-- selectRandomindecies, 
-- getRandomBetween

prop_getCell :: Region -> Bool
prop_getCell r = True

k = generate (arbitrary :: Gen Numm)

--------------------------------------
--           Run All tests          --

---- Property based tests
--return [] -- Yikes!

--runTests :: IO Bool
--runTests = $quickCheckAll

---- Unit tests
--runUnits :: IO TH.Counts
--runUnits = TH.runTestTT tests
