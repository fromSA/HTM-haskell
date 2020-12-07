{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Numeric
-- Description : An encoder, tranforms integer values to SDR.
-- Copyright   : (c) Fromsa Hera, 2020
--                   Numenta, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module is contains the configuration record for an numeric encoder.
-- It also provides a function 'encode' that transforms an integer value to an SDR.
module HTM.Encoder.Numeric where

import Control.Lens (makeLenses, (^.))
import GHC.Generics (Generic)
import GHC.Natural (Natural, naturalToInt)
import HTM.SDR (SDR (..), SDRRange (..))

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

checkEncoderInvariant :: EncoderConfig -> Bool
checkEncoderInvariant c =
  _maxVal c >= _minVal c
    && _buckets c > 0
    && _bitsPerBucket c > 0
    && _maxVal c - _minVal c >= naturalToInt (_buckets c)

-- | Given the configuration for the encoder, returns the range of possible bit indecies of the SDR.
getRange :: EncoderConfig -> Maybe SDRRange
getRange conE =
  if checkEncoderInvariant conE
    then
      Just
        SDRRange
          { _minIndex = 0,
            _maxIndex = sum [conE ^. buckets, conE ^. bitsPerBucket] - 1
          }
    else Nothing

-- | Encodes an input-value as an SDR.
--
-- >>> let encoder = EncoderConfig Numeric 2 10 8 1
-- >>> encode 2 encoder
-- Just (SDR {_sdr = [0], _sdrRange = SDRRange {_minIndex = 0, _maxIndex = 8}})
--
-- >>> let encoder = EncoderConfig Numeric 2 10 8 4
-- >>> encode 2 encoder
-- Just (SDR {_sdr = [0,1,2,3], _sdrRange = SDRRange {_minIndex = 0, _maxIndex = 11}})
--
-- >>> let encoder = EncoderConfig Numeric 2 10 8 1
-- >>> encode 1 encoder
-- Nothing
--
-- >>> let encoder = EncoderConfig Numeric 2 10 8 1
-- >>> encode 11 encoder
-- Nothing
encode :: Int -> EncoderConfig -> Maybe SDR
encode val config =
  if val <= config ^. maxVal && val >= config ^. minVal
    then
      let start = getStartOf val config
       in ( \range ->
              Just
                SDR
                  { _sdr = [start + i | i <- [0 .. (config ^. bitsPerBucket -1)]],
                    _sdrRange = range
                  }
          )
            =<< getRange config
    else Nothing

-- | Get the encoding start position of a value in the SDR.
getStartOf :: Int -> EncoderConfig -> Natural
getStartOf n config = floor $ realToFrac (naturalToInt (config ^. buckets) * (n - config ^. minVal)) / realToFrac (config ^. maxVal - config ^. minVal)

-- | The total number of bits used in a SDR.
totNrBits :: EncoderConfig -> Natural
totNrBits config = sum (map ($ config) [(^. buckets), (^. bitsPerBucket)]) - 1
