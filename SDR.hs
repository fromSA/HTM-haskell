module SDR (SDR, SDRConfig(..), SDRRange(..), BitIndex(..), encode, totNrBits) where

-- |
type BitIndex = Int

-- |A list of bit indecies. Used as a representaion of an input value, like an integer, class or other data type. 
-- The values are transformed to SDR using an encoder
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

-- |The range of an SDR.
data SDRRange = SDRRange{
  minIndex  :: BitIndex -- ^The smallest sdr bit index. TODO remove this, because it is implisit = 0
  ,maxIndex :: BitIndex -- ^The largest sdr bit index.
}

-- |The SDR config for the input value. Here, the input is a single integer between minVal and maxVal
data SDRConfig = SDRConfig{
  minVal          :: Int -- ^The minimum possible value of the input value.
  , maxVal        :: Int -- ^The maximum possible value of the input value.
  , buckets       :: Int -- ^The inputvalues are grouped into buckets, where each bucket represents 1 or more input values.
  , bitsPerBucket :: Int -- ^Each bucket is encoded with this number of bits in the inputSDR.
  , sdrRange      :: SDRRange -- ^ The range of the input sdr. Bounded between 0 and an upper value n.
}

-- |The total number of bits used in a SDR.
totNrBits :: SDRConfig -> Int
totNrBits config = sum (map ($ config) [buckets, bitsPerBucket]) - 1

-- |Encodes an inputvalue as an SDR.
encode :: Int -> SDRConfig -> SDR
encode n config = let start = getStartOf n config in
  [start + i | i <- [0..(bitsPerBucket config - 1)]]

-- |Get the encoding start position of a value in the SDR.
getStartOf :: Int -> SDRConfig -> Int
getStartOf n config = floor $  realToFrac ((buckets config) * (n - (minVal config))) / (realToFrac ((maxVal config) - (minVal config)))
