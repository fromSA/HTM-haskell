-- |
-- Module      : REPL
-- Description : An example use of this package
-- Copyright   : (c) Fromsa Hera, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This modules contains an example use of this package. Just run 'main' in @cabal repl@ or @ghci@.
module REPL(initConfigs, initHTMConfig, initRegionConfig, initEncoderConfig) where

import Control.Lens ((^.))
import System.Random (mkStdGen)
import System.Random.Shuffle ()
import SRC.Encoder.Config
import SRC.Encoder.Numeric
  ( encode,
    getRange,
  )
import SRC.HTM.Config
import SRC.HTM.HTM
import SRC.Region.Region
import SRC.Package
import SRC.SDR
import System.Exit (exitFailure)
import Control.Monad (when)

import Diagrams.Prelude 
import Diagrams.Backend.SVG
import Diagrams.Size

initEncoderConfig :: EncoderConfig
initEncoderConfig =
  EncoderConfig
    { _encoderType = Numeric,
      _minVal = 0,
      _maxVal = 100,
      _buckets = 50,
      _bitsPerBucket = 20
    }

initRegionConfig :: RegionConfig
initRegionConfig =
  RegionConfig
    { _nrOfColumns = 100,
      _nrOfCellsPerColumn = 2,
      _initNrOfFeedForwardSynpases = 40,
      _nrOfSynapsesPerSegment = 4,
      _mappingType = Random,
      _initConnectionStrength = 0.7,
      _mvWindow = 3,
      _initRad = 5
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
            _learningEnabled = True,
            _selfPredict = True
          }
    }


initConfigs :: IO Package
initConfigs = do
  let s = initEncoderConfig
  case getRange s of
    Just a -> do
      let r = initRegionConfig
      let h = initHTMConfig
      return $
        Package
          { _conH = h,
            _conR = r,
            _conS = Nothing,--s,
            _value = SDR [] a,
            _randomGenerator = mkStdGen 12
          }
    Nothing -> exitFailure


saveToFile :: Int -> SDR -> IO()
saveToFile v s = do
        let str = show v ++ ", " ++ show (s^.sdr) ++ "\n"
        appendFile "Output/OutputFile.txt" str


filePath = "Output/c.svg"
figSize = mkSizeSpec2D (Just 2700) (Just 900)
row ss = renderSVG filePath figSize $ visualize ss

toSVG f = renderSVG f figSize


columnss :: [Diagram B] -> Diagram B
columnss (x:xs) = foldl (===) x xs

main2 :: IO()
main2 = do 
  let conS = initEncoderConfig
  case encode conS 20 of 
    Just p -> 
      row p
    Nothing ->
      print "No ouput"

main :: IO ()
main = do
  -- Configerations
  package <- initConfigs

  -- Initialisation
  region <- initRegion (package^. conS) (package^.conR)

  -- InputData
  let d1 = concat (replicate 35 [(2,14), (2,2)])
  let d2 = concat (replicate 35 [(2,14), (2,33)])
  let d3 = concat (replicate 35 [(2,77), (2,55)])
  let seqData = concat [replicate a b | (a,b) <- [(2,14), (2,12)] ++ d1 ++ d2 ++ d3]

  -- Learning/Predicting, i.e. spatial and temporal poolers
  s <- compute ([],[]) seqData package region

  toSVG "Output/sdr.svg" $ columnss (fst s)

  toSVG "Output/region.svg" $ columnss (snd s)

saveStrToFile :: FilePath -> Int -> String -> IO ()
saveStrToFile f v s = do
  let str = show v ++ ", " ++ s ++ "\n"
  appendFile f str

-- | Apply HTM on a sequence of input data.
compute :: ([Diagram B],[Diagram B]) -> [Int] -> Package -> Region -> IO ([Diagram B],[Diagram B])
compute (ss,rs) [] _ _ = do
  putStrLn "encoding completed"
  return (ss,rs)

compute (ss,rs) (x:xs) p region = do
  putStrLn $ "----- next encoding. val: " ++ show x
  let encodedSDR = Nothing --encode (p^.conS) x
  case encodedSDR of
    Just val ->
      do
        let s = visualize val
        -- print val
        -- save encoding to file
        --saveToFile x val
        -- Spatial Encoding
        let regionSpat = spatialPooler p {_value = val} region -- Updates columnState among other variables

        -- Temporal encoding
        regionTemp <- temporalPooler p regionSpat -- Updates cellState among other variables
        -- putStrLn $ displayRegion regionTemp -- This shows the state of all cells

        --saveStrToFile "Output/region.txt" x $ displayRegion regionTemp

        let r = renderRegion regionTemp
        
        compute (ss++[s],rs++[r])  xs p $ switch regionTemp

    Nothing -> do
      print "Invalid value"
      return (ss,rs)
      
    

