-- |
-- Module      : Main
-- Description : Read evaluate print loop for running HTM algorithm on a sequence of inputs.
-- Copyright   : (c) Fromsa Hera, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an REPL interface for running the HTM algorithm. Just run 'main' in @cabal repl@ or @ghci@.
module REPL where

-- import           Data.Maybe
-- import           HaskellSay (haskellSay)

import Control.Lens ()
import Debug.Trace ()
import HTM.Encoder.Numeric
import HTM.HTM
import HTM.SDR
import System.Random ()
import System.Random.Shuffle ()

{-
1. init encoder config
2. init region config
3. init htm config
4. init display config
5. input a value >> eval input >> print display >> repeat 5

-}


main :: IO ()
main = do
  -- Configerations
  let sdrConfig = initEncoderConfig
  let regionConfig = initRegionConfig
  let htmConfig = initHTMConfig

  let package = initPackage htmConfig regionConfig sdrConfig
  -- Initialisation
  region <- initRegion sdrConfig regionConfig

  -- InputData
  let seqData = [20, 20, 20, 20, 20, 20, 20, 20]

  -- Learning/Predicting, i.e. spatial and temporal poolers
  compute seqData package region

-- | Apply HTM on a sequence of input data.
compute :: [Int] -> Package -> Region -> IO ()
compute [] _ region = do
  putStrLn "last encoding"
  print region
compute (x : xs) p region = do
  putStrLn "----- next encoding"
  let encodedSDR = encode x (_conS p)

  case encodedSDR of
    Just val ->
      do
        --putStrLn "spatial began"
        let regionSpat = spatialPooler p {_value = val} region

        print regionSpat
        --putStrLn "temporal began"

        regionTemp <- temporalPooler p regionSpat -- There is an eternal loop here
        --print regionTemp
        compute xs p regionTemp
    Nothing ->
      print "Invalid value"

initPackage :: HTMConfig -> RegionConfig -> EncoderConfig -> Package
initPackage h r s =
  Package
    { _conH = h,
      _conR = r,
      _conS = s,
      _value = SDR [] $ getRange s
    }

initEncoderConfig :: EncoderConfig
initEncoderConfig =
  EncoderConfig
    { _encoderType = Numeric,
      _minVal = 0,
      _maxVal = 100,
      _buckets = 50,
      _bitsPerBucket = 30
    }

initRegionConfig :: RegionConfig
initRegionConfig =
  RegionConfig
    { _nrOfColumns = 100,
      _nrOfCellsPerColumn = 2,
      _maxNrOfInputBits = 2,
      _nrOfSynapsesPerSegment = 4,
      _mappingType = Random,
      _initConnectionStrength = 0.7,
      _mvWindow = 3
    }

initHTMConfig :: HTMConfig
initHTMConfig =
  HTMConfig
    { _spatialConfig =
        SpatialConfig
          { _overlapThreshold = 2,
            _mop = 0.2,
            _proxSynConInc = 0.2,
            _proxSynConDec = 0.2,
            _pConthresh = 0.2,
            _colActLev = 1
          },
      _temporalConfig =
        TemporalConfig
          { _targetDensity = 0.3,
            _boostStrength = 0.3,
            _connectedPermenance = 0.5,
            _activationThreshold = 2,
            _predictedDecrement = 0.1,
            _permanenceIncrement = 0.2,
            _permanenceDecrement = 0.2,
            _learningThreshold = 2,
            _learningEnabled = True
          }
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

neigh :: Int -> Int -> [a] -> [a]
neigh at rad = take rad . drop (at - (rad `div` 2))
-}


intro :: IO ()
intro = do
  print "Welcome, this is an interface for running HTM algorithm.\
        \ If you want to break, type .quit"

initEncoder :: IO ()
initEncoder = do
  print "Lets configure the encoder."

prompt :: String -> IO String
prompt s = putStrLn s >> getLine

eval :: [(a, String)] -> (a -> IO ()) ->  IO ()
eval [] _ = return ()
eval (x:_) f = f (fst x)

chooseType :: IO ()
chooseType = do
  num <- prompt "1. Numberic\
  \\n2. Categorical (Not implemented)\
  \\nchoose type by number:"
  let val = reads num :: [(Int, String)]
  eval val print



{- _encoderType = Numeric,
      _minVal = 0,
      _maxVal = 100,
      _buckets = 50,
      _bitsPerBucket = 30
    -}
  
