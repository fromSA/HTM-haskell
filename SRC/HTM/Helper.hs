{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module SRC.HTM.Helper where

import SRC.Package
import SRC.Region.Region (Column)
import SRC.SDR (SDR, SDRRange (..), maxIndex, minIndex, sdr)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import GHC.Generics (Generic)
import Control.Lens(makeLenses, (^.))
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
newtype Region2 = Region2 {_cols :: [Column]} deriving (Eq, Show, Generic)
makeLenses ''Region2

-- | Construct a new region2.
-- This function returns an IO Region monad because it uses the StdRandom as a random generator.
initRegion2 :: Maybe SDRRange -> RegionConfig -> IO Region2
initRegion2 conS conR = Region2 . (^. currentStep) <$> initRegion conS conR

data Duplicate = Duplicate Region2 Region2 deriving (Eq, Show, Generic)
makeLenses ''Duplicate

data GateInputRegions = GateInputRegions {
    _proximalRegs :: Region2,
    _distalRegs :: [Region2]
    }   deriving (Eq, Show, Generic)

makeLenses ''GateInputRegions



data WinnerCells = WinnerCells {
    _prox :: [Cell],
    _dist :: [[Cell]]
    }
makeLenses ''WinnerCells

newtype PreviousColumns = PreviousColumns [Column]


data DendriteType = Proximal | Distal Int 

data Location a = Location {
    _cellIndex :: Maybe Int,
    _dendriteType :: Maybe DendriteType,
    _segmentIndex :: Maybe Int, 
    _bestMatch :: Maybe a 
}

makeLenses ''Location