{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

-- | This module represents an `SDR` as defined by (Numenta).
-- It represents a ...
module HTM.SDR where --(SDR, SDRConfig(..), SDRRange(..), BitIndex(..), encode, totNrBits) where

import Control.Lens ( (^.), makeLenses )
import Debug.Trace ()
import GHC.Natural ( Natural, naturalToInt )
import HTM.CommonDataTypes ( BitIndex )

-- | A list of bit indecies. Used as a representaion of an input value, like an integer, class or other data type.
--  The values are transformed to SDR using an encoder
type SDR = [BitIndex]

data Encoder = Numeric | Categorical

{- Numeric Range
  | NumbericLog -- These are types of encodings.
  | Delta
  | Category Cyclic Order
  | Geospatial Range Speed
  | Text

data Range = Bounded | UnBounded
data InputValue = Number | Vector
data Number = Continues Range | Discrete Range
-}

-- | The range of an SDR.
data SDRRange = SDRRange
  { -- | The smallest sdr bit index. TODO remove this, because it is implisit = 0
    _minIndex :: BitIndex,
    -- | The largest sdr bit index.
    _maxIndex :: BitIndex
  }

makeLenses ''SDRRange

-- | The SDR config for the input value. Here, the input is a single integer between minVal and maxVal
data SDRConfig = SDRConfig
  { -- | The minimum possible value of the input value.
    _minVal :: Int,
    -- | The maximum possible value of the input value.
    _maxVal :: Int,
    -- | The inputvalues are grouped into buckets, where each bucket represents 1 or more input values.
    _buckets :: Natural,
    -- | Each bucket is encoded with this number of bits in the inputSDR.
    _bitsPerBucket :: Natural,
    -- | The range of the input sdr. Bounded between 0 and an upper value n.
    _sdrRange :: SDRRange
  }

makeLenses ''SDRConfig

-- | The total number of bits used in a SDR.
totNrBits :: SDRConfig -> Natural
totNrBits config = sum (map ($ config) [(^. buckets), (^. bitsPerBucket)]) - 1

-- | Encodes an inputvalue as an SDR.
encode :: Int -> SDRConfig -> SDR
encode n config =
  let start = getStartOf n config
   in [start + i | i <- [0 .. (config ^. bitsPerBucket - 1)]]

-- | Get the encoding start position of a value in the SDR.
getStartOf :: Int -> SDRConfig -> Natural
getStartOf n config = floor $ realToFrac (naturalToInt (config ^. buckets) * (n - config ^. minVal)) / realToFrac (config ^. maxVal - config ^. minVal)
