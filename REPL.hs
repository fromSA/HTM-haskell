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
module REPL(main) where

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
      _initNrOfFeedForwardSynpases = 20,
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
            _learningEnabled = True
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
            _conS = s,
            _value = SDR [] a,
            _randomGenerator = mkStdGen 12
          }
    Nothing -> exitFailure


saveToFile :: Int -> SDR -> IO()
saveToFile v s = do
        let str = show v ++ ", " ++ show (s^.sdr) ++ "\n"
        appendFile "Output/OutputFile.txt" str


filePath = "Output/c.svg"
figSize = mkSizeSpec2D (Just 400) (Just 400)
row ss = renderSVG filePath figSize $ visualize ss

renderFig = renderSVG filePath figSize


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
  let seqData = [20, 20, 20, 20, 20, 20, 20, 20, 77, 77, 77, 77, 77, 77, 77, 30, 30, 30, 30]

  -- Learning/Predicting, i.e. spatial and temporal poolers
  rows <- compute [] seqData package region

  renderFig $ columnss rows

saveStrToFile :: FilePath -> Int -> String -> IO ()
saveStrToFile f v s = do
  let str = show v ++ ", " ++ s ++ "\n"
  appendFile f str

-- | Apply HTM on a sequence of input data.
compute :: [Diagram B] -> [Int] -> Package -> Region -> IO [Diagram B]
compute ds [] _ _ = do
  putStrLn "encoding completed"
  return ds

compute ds (x:xs) p region = do
  putStrLn $ "----- next encoding. val: " ++ show x
  let encodedSDR = encode (p^.conS) x
  case encodedSDR of
    Just val ->
      do
        let d = visualize val
        -- print val
        -- save encoding to file
        --saveToFile x val
        -- Spatial Encoding
        let regionSpat = spatialPooler p {_value = val} region -- Updates columnState among other variables

        -- Temporal encoding
        regionTemp <- temporalPooler p regionSpat -- Updates cellState among other variables
        putStrLn $ displayRegion regionTemp -- This shows the state of all cells

        --saveStrToFile "Output/region.txt" x $ displayRegion regionTemp
        
        compute (d:ds) xs p $ switch regionTemp

    Nothing -> do
      print "Invalid value"
      return ds
      
    
  

