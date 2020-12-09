{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module SRC.Encoder.Config where

import Control.Lens (makeLenses)
import GHC.Generics (Generic)
import GHC.Natural (Natural)

-- | Defined the type of Encoder
data EncoderType
  = -- | A numeric encoder, encodes a sequence of bounded integers.
    Numeric
  | -- | A categorical encoder, encodes a bounded set of independet values. OBS. Not implemented yet.
    Categorical
  deriving (Enum, Show, Generic)

-- | The SDR config for the input-value. Here, the input is a single integer between minVal and maxVal
data EncoderConfig = EncoderConfig
  { -- | The kind of encoder to use. This depends on the type of data you plan to work with.
    _encoderType :: EncoderType,
    -- | The minimum possible value of the input-value.
    _minVal :: Int,
    -- | The maximum possible value of the input-value. Must be >= _minVal
    _maxVal :: Int,
    -- | The input-values are grouped into buckets, where each bucket represents 1 or more input-values.
    _buckets :: Natural,
    -- | Each bucket is encoded with this number of bits in the inputSDR.
    _bitsPerBucket :: Natural
  }
  deriving (Show, Generic)

makeLenses ''EncoderConfig

defualtEncoderConfig :: EncoderConfig
defualtEncoderConfig =
  EncoderConfig
    { _encoderType = Numeric,
      _minVal = 0,
      _maxVal = 9,
      _buckets = 5,
      _bitsPerBucket = 3
    }