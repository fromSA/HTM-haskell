module Main where
import           Data.Maybe
import           HaskellSay (haskellSay)
import           HTM
import Region
import SDR
import MovingAverage
import           System.Random
import Data.List (sortOn)
import System.Random.Shuffle
import Control.Lens
-- TODO
-- Clean up and improve Code
-- Add a function that creates a segment
-- Implement temporal pooler
-- TODO perhaps use a rotational representation?

main :: IO ()
main = do

  -- Random generator
  gen <- newStdGen 

  -- Configerations
  let sdrConfig = initSDRConfig 
  let regionConfig = initRegionConfig
  let htmConfig = initHTMConfig

  let package = initPackage htmConfig regionConfig sdrConfig
  -- Initialisation
  region <- initRegion sdrConfig regionConfig

  -- InputData
  let seqData = [50,20,25,20]

  -- Learning/Predicting, i.e. spacial and temporal poolers
  compute seqData package region

  

-- main = print <$> spacialPooler <*> (encode 12 sdrConfig) <*> (initRegion initConfig initSDRConfig)


compute :: [Int] -> Package -> Region -> IO ()
compute [] p region = print region
compute (x:xs) p region = do
  print region
  let encodedSDR = encode x (_conS p)
  let regionSpat =  spacialPooler p{_sdr = encodedSDR} region
  --regionTemp <- temporalPooler (_conH p) (_conR p) regionSpat -- There is an eternal loop here
  compute xs p regionSpat

initPackage :: HTMConfig -> RegionConfig -> SDRConfig -> Package
initPackage h r s = Package{
  _conH = h, 
  _conR = r, 
  _conS = s, 
  _sdr = []
}

initSDRConfig :: SDRConfig
initSDRConfig = SDRConfig{
  minVal          = 0
  , maxVal        = 100
  , buckets       = 50
  , bitsPerBucket = 30
  , sdrRange = SDRRange {minIndex  = 0, maxIndex = sum [50{- bucket -}, 30{- bitsPerBucket -}] - 1}
}

initRegionConfig :: RegionConfig
initRegionConfig = RegionConfig{
  _nrOfColumns = 100
  , _nrOfCellsPerColumn = 1
  , _maxNrOfInputBits = 3
  , _nrOfSynapsesPerSegment = 4
  , _mappingType = Random
  , _initConnectionStrength = 1.0
  , _mvWindow = 3
}

initHTMConfig :: HTMConfig
initHTMConfig = HTMConfig{
  _overlapThreshold = 0
  , _pConthresh = 0.2
  , _colActLev = 1
  , _proxSynConInc = 0.2
  , _proxSynConDec = 0.2
  , _mop = 0.2
  , _targetDensity = 0.3
  , _boostStrength = 0.3
  , _connectedPermenance = 0.5
  , _activationThreshold = 2 
  , _predictedDecrement = 0.1
  , _permanenceIncrement = 0.1
  , _permanenceDecrement = 0.1
  , _learningThreshold = 2
  , _learningEnabled = True
}

{-
main = do
  gen <- newStdGen
  let a = shuffle' [1,2,3,4,5,6] 6 gen
  print a

  --let a = getFirst [(0,5),(2,5),(3,5),(1,5)]
  --print a
  --haskellSay "Hello, Haskell!"

-- Unused Code from HTM
{-
initSensorySynapses :: Int -> Config -> SDR -> [SensorySynapse]
initSensorySynapses index config
  | mappingSDRToRegion config == Random = map (\x -> (x, initConnectionStrength config)) $ selectRandom index config $ inputfield config --TODO choose random subset of inputfield portal choose init connection strength

-- select Random pixels of the sensory inputField
selectRandom :: Int -> Config -> SDRRange -> [BitIndex]
selectRandom index config ls = randElements (potentialNrOfSensorySynapses config) index ls

randElements :: Int -> Int -> [a] -> [a]
randElements n index ls
  | n < 0 = []
  | n == 0 = []
  | n > 0 = randElem index ls : randElements (n-1) index ls

randElem :: Int -> [a] -> a -- does not work for empty list
randElem index ls =  let g = mkStdGen index in
                          (\l g -> l !! fst (randomR (0, length l) g)) ls g

slidingWindow index ls = drop (index) $ take (index + 10) ls
wrappedSlidingWindow index ls = drop (index) $ take (index + 10) ls

-}

rollDice :: Int -> Int -> IO Int
rollDice mi ma = getStdRandom (randomR (mi,ma))

rollTwice a b = rollDice a b >>= print >> rollDice 1 2 >>= print

getFirst :: [(Int,Int)] -> [(Int,Int)]
getFirst xs = take 2 $ sortOn fst xs


data Ob = Pointer {v :: Int, p:: Ob} | Base deriving(Show) 
-}









