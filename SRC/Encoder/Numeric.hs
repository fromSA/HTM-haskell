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
module SRC.Encoder.Numeric where

import Control.Lens ((^.))
import GHC.Natural (Natural, intToNatural, naturalToInt)
import SRC.SDR (SDR (..), SDRRange (..))
import SRC.Encoder.Config 

-- | As the name indicates, this makes sure the encoder creates a valid SDR and is a bounded numeric encoder
checkEncoderInvariant :: EncoderConfig -> Bool
checkEncoderInvariant c =
  _maxVal c >= _minVal c
    && _buckets c > 0
    && _bitsPerBucket c > 0
    && (_maxVal c - _minVal c) + 1 >= naturalToInt (_buckets c)

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
encode :: EncoderConfig -> Int -> Maybe SDR
encode config val =
  if validInputValue config val
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

validInputValue :: EncoderConfig -> Int -> Bool
validInputValue c i = i <= c ^. maxVal && i >= c ^. minVal

-- | Get the encoding start position of a value in the SDR.
getStartOf :: Int -> EncoderConfig -> Natural
getStartOf n config = computeStart n (naturalToInt (config ^. buckets)) (config ^. minVal) (config ^. maxVal)

--floor $ realToFrac (naturalToInt (config ^. buckets) * (n - config ^. minVal)) / realToFrac (config ^. maxVal - config ^. minVal)

computeStart :: Int -> Int -> Int -> Int -> Natural
computeStart n buckets minVal maxVal 
    | n > maxVal =  computeStart maxVal buckets minVal maxVal
    | n < minVal =  computeStart minVal buckets minVal maxVal
    | otherwise = intToNatural $ floor $ realToFrac (buckets * (n - minVal)) / realToFrac (maxVal + 1 - minVal)

-- | The total number of bits used in a SDR.
totNrBits :: EncoderConfig -> Natural
totNrBits config = computeTotalNumberOfBits (config ^. buckets) (config ^.bitsPerBucket)

computeTotalNumberOfBits :: Natural -> Natural -> Natural
computeTotalNumberOfBits buckets bitsPerBucket = buckets + bitsPerBucket - 1