{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Lens ((^.))
import Debug.Trace ()
import GHC.Natural (Natural, intToNatural)
import HTM.Encoder.Numeric
import HTM.HTM
import HTM.SDR
import System.Random ()
import System.Random.Shuffle ()
import Data.Char (isSpace)
import System.Exit (exitSuccess)

{- TODO

1. init encoder config
2. init region config
3. init htm config
4. init display config
5. input a value >> eval input >> print display >> repeat 5

-}

{-main :: IO ()
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
      _initNrOfFeedForwardSynpases = 2,
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
-}
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

compute' :: Package -> Region -> IO ()
compute' p region = do
  -- TODO read in value -> encode or break -> 
  x <- chooseNum "HTM << value: " Nothing Nothing
  let encodedSDR = encode x (_conS p)
  case encodedSDR of
    Just val ->
      do
        --putStrLn "spatial began"
        let regionSpat = spatialPooler p {_value = val} region

        -- print regionSpat
        --putStrLn "temporal began"

        regionTemp <- temporalPooler p regionSpat -- There is an eternal loop here
        --print regionTemp
        compute' p regionTemp
    Nothing -> do
      print "Invalid value is not encoded."
      compute' p region



main :: IO ()
main = do
  putStrLn
    "Welcome, this is an interface for running the HTM algorithm.\
    \ If you want to break, type .q"
  package <- initConfigs
  -- TODO display config, save config
  region <- initRegion (_conS package) (_conR package)
  putStrLn ">>> Configuration complete. Running HTM algorithm >"
  compute' package region 

initConfigs :: IO Package
initConfigs = do
  putStrLn "Lets configer the model"
  s <- initEncoderCon
  r <- initRegionCon s
  h <- initHTMCon r
  return $
    Package
      { _conH = h,
        _conR = r,
        _conS = s,
        _value = SDR [] $ getRange s
      }


data InputState a = Break | WrongInput | NoInput | Success a

-- TODO Make sure to inform about type of number to input; integer or float
readValue :: (Read a, Show a, Ord a, Num a) => String -> String -> String -> (a -> Bool) -> IO (InputState a)
readValue initMsg wrongInputMsg noInputMsg inputChecker = do
  putStr $ initMsg ++ " "
  input <- getLine
  if trim input == ".q" 
    then exitSuccess
    else do
      let ms = reads input
      let size = length ms
      let val = if size /= 0 then Just (fst (head ms)) else Nothing
      case val of
        Just m ->
          if inputChecker m
            then return $ Success m
            else putStrLn wrongInputMsg >> return WrongInput
        Nothing -> putStrLn noInputMsg >> return NoInput

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

withMaxBound :: (Read a, Show a, Ord a, Num a) => String -> a -> a -> IO a
withMaxBound msg i j = do
  state <- readValue ("\nChoose a number between " ++ show i ++ " and " ++ show j ++ ":") "Invalid value, try Again.\n" "" (\x -> i <= x && x <= j)
  case state of
    Success m -> return m
    _ -> chooseNum msg (Just i) (Just j)

withMinBound :: (Read a, Num a, Ord a, Show a) => String -> a -> Maybe a -> IO a
withMinBound msg i b =
  case b of
    Just j -> do
      withMaxBound msg i j
    Nothing -> do
      state <- readValue ("\nChoose a number >= " ++ show i ++ ":") "Invalid value, try Again.\n" "Invalid value, try Again.\n" (i <=)
      case state of
        Success m -> return m
        _ -> chooseNum msg (Just i) b

chooseNum :: (Read a, Num a, Ord a, Show a) => String -> Maybe a -> Maybe a -> IO a
chooseNum msg a b = do
  putStrLn $ "\n> " ++ msg
  case a of
    Just i ->
      withMinBound msg i b
    Nothing -> do
      state <- readValue "\nChoose a number:" "Invalid value, try Again.\n" "Invalid value, try Again.\n" (const True)
      case state of
        Success m -> return m
        _ -> chooseNum msg a b

initRegionCon :: EncoderConfig -> IO RegionConfig
initRegionCon e = do
  putStrLn "\n\n>>> Region configuration:"
  initNrOfFeedForwardSynpases_ <- chooseNum "initNrOfFeedFarwardSynapses: Maximum number of input bits connected to a column" (Just 0) (Just (getRange e ^.maxIndex))
  nrOfColumns_ <- chooseNum "nrOfColumns: Number of columns" (Just 0) Nothing
  nrOfCellsPerColumn_ <- chooseNum "nrOfCellsPerColumn: Number of cells per column" (Just 0) Nothing
  nrOfSynapsesPerSegment_ <- chooseNum "nrOfSynapsesPerSegment: Number of synapses per segment" (Just 0) Nothing
  initConnectionStrength_ <- chooseNum "initConnectionStrength: The initial connection strength of a synapse" (Just (0.0 :: Float)) Nothing
  mvWindow_ <- chooseNum "mvWindow: Moving average window size" (Just 0) Nothing
  mappingType_ <- chooseType "mappingType: MappingType between inputspace and Region" [(Random, "Randomly connect inputbits to columns")]

  return $
    RegionConfig
      { _nrOfColumns = nrOfColumns_,
        _nrOfCellsPerColumn = nrOfCellsPerColumn_,
        _initNrOfFeedForwardSynpases = initNrOfFeedForwardSynpases_,
        _nrOfSynapsesPerSegment = nrOfSynapsesPerSegment_,
        _mappingType = mappingType_,
        _initConnectionStrength = initConnectionStrength_,
        _mvWindow = mvWindow_
      }

initHTMCon :: RegionConfig -> IO HTMConfig
initHTMCon r = do
  putStrLn "\n\n>>> HTM configuration: "
  
  
  putStrLn "\n>> Spatial-pooler configuration:"
  
  overlapThreshold_ <- chooseNum "Overlap threshold: The number of active feedforward synpases for a column to activate." (Just 0) (Just (r^.initNrOfFeedForwardSynpases))
  mop_ <- chooseNum "mop: Columns with less moving-average overlap score percentage that this threshold are boosted." (Just (0.0 :: Float)) Nothing
  proxSynConInc_ <- chooseNum "proxSynConInc: Incremental value of a feedforward synapse" (Just (0.0 :: Float)) (Just (1.0 :: Float))
  proxSynConDec_ <- chooseNum "proxSynConDec: Decremental value of a feedforward synapse" (Just (0.0 :: Float)) (Just (1.0 :: Float))
  pConthresh_ <- chooseNum "pConthresh: Permenance connection threshold value of a synapse connected to the inputspace" (Just (0.0 :: Float)) (Just (1.0 :: Float))
  colActLev_ <- chooseNum "colActLev: The number of columns that should be activated within the inhibition radius." (Just 0) (Just (r^.nrOfColumns))

  putStrLn "\n>> Temporal-pooler configuration:"

  targetDensity_ <- chooseNum "targetDensity: The desired percent of active duty cycle within the sliding window." (Just (0.0 :: Float)) (Just (1.0 :: Float))
  boostStrength_ <- chooseNum "boostStrength: The degree of change for the update of '_boost'." (Just (0.0 :: Float)) (Just (1.0 :: Float))
  connectedPermenance_ <- chooseNum "connectedPermance: The threshold for permenant synaptic connection." (Just (0.0 :: Float)) (Just (1.0 :: Float))
  activationThreshold_ <- chooseNum "activationThreshold: The number of synapses per segment that must be active for the segment to be considered active." (Just (0 :: Natural)) (Just (r ^. nrOfSynapsesPerSegment))
  predictedDecrement_ <- chooseNum "predictedDecrement: The punishment strength, used to punish wrongly predicted segments" (Just 0) (Just (1.0 :: Float))
  permanenceDecrement_ <- chooseNum "permanenceDecrement: The forgetting strength, determines how fast a pattern is forgotten by the region" (Just (0.0 :: Float)) (Just (1.0 :: Float))
  permanenceIncrement_ <- chooseNum "permanenceIncrement: The learning strength, determines how fast a pattern is learned by the region" (Just (0.0 :: Float)) (Just (1.0 :: Float))
  learningThreshold_ <- chooseNum "learningThreshold: The threshold for when a cell is considered matching, i.e. when a segment has this many activeconnections to the previously active/winner cells." (Just (0 :: Natural)) (Just (r ^. nrOfColumns * r ^. nrOfCellsPerColumn))
  learningEnabled_ <- chooseType "learningEnabled: If true, the synapse connections are  activated." [(True, ""), (False, "")]

  return $
    HTMConfig
      { _spatialConfig =
          SpatialConfig
            { _overlapThreshold = overlapThreshold_,
              _mop = mop_,
              _proxSynConInc = proxSynConInc_,
              _proxSynConDec = proxSynConDec_,
              _pConthresh = pConthresh_,
              _colActLev = colActLev_
            },
        _temporalConfig =
          TemporalConfig
            { _targetDensity = targetDensity_,
              _boostStrength = boostStrength_,
              _connectedPermenance = connectedPermenance_,
              _activationThreshold = activationThreshold_,
              _predictedDecrement = predictedDecrement_,
              _permanenceIncrement = permanenceIncrement_,
              _permanenceDecrement = permanenceDecrement_,
              _learningThreshold = learningThreshold_,
              _learningEnabled = learningEnabled_
            }
      }

initEncoderCon :: IO EncoderConfig
initEncoderCon = do
  putStrLn "\n\n>>> SDR-encoder configuration:"
  encoderType_ <- chooseType "EncoderType: how to convert a raw value into and SDR" [(Numeric, "A numeric encoder, encodes a sequence of bounded integers."){-, (Categorical, "A categorical encoder, encodes a bounded set of independet values. OBS. Not implemented yet.")-}]
  minVal_ <- chooseNum "MinVal: The minimum input value" (Just 0) Nothing
  maxVal_ <- chooseNum "MaxVal: The maximum input value" (Just minVal_) Nothing
  buckets_ <- chooseNum "Buckets: The number of consecutive input values encoded as the same SDR" (Just (0 :: Natural)) (Just $ intToNatural $ maxVal_- minVal_)
  bitsPerBucket_ <- chooseNum "BitsPerBucket: The number of bits representing a single bucket" (Just 1) Nothing
  return $
    EncoderConfig
      { _encoderType = encoderType_,
        _minVal = minVal_,
        _maxVal = maxVal_,
        _buckets = buckets_,
        _bitsPerBucket = bitsPerBucket_
      }

promptOptions :: (Show a) => [(a, String)] -> String
promptOptions = promptOptions' 0
  where
    promptOptions' _ [] = "\n\nChoose the type by number: "
    promptOptions' n (x : xs) =
      "\n" ++ show n ++ ") " ++ show (fst x) ++ ": \t" ++ snd x ++ promptOptions' (n + 1) xs

chooseType :: (Show a) => String -> [(a, String)] -> IO a
chooseType param xs = do
  putStrLn $ "> " ++ param
  chooseType' xs
  where
    chooseType' xs = do
      let initMsg = promptOptions xs
      let size = length xs
      state <- readValue initMsg "Choose one of the options presented.\n" "Choose one of the options presented.\n" (\x -> x < size && x >= 0)
      case state of
        Success m -> return $ fst $ xs !! m
        _ -> chooseType' xs
