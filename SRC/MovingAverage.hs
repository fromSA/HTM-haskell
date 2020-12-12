{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : MovingAverage
-- Description : A moving average data structure.
-- Copyright   : (c) Fromsa Hera, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module is used to calculate a moving average. It comes with a record that contains the data
-- and functions that produce some statistic on the data, like the moving average, moving percentage, etc.
module SRC.MovingAverage
  ( MovingAverage (..),
    Bit,
    window,
    bits,
    on,
    off,
    average,
    sparsity
  )
where

import Control.Lens (makeLenses, (%~), (&), (^.))
import GHC.Natural (Natural, intToNatural)
import GHC.Generics (Generic)

-- -------------------------------------------------------------
--                           DATA
-- -------------------------------------------------------------
-- | A bit is represented by a boolean value.
type Bit = Bool

-- | This record maintains the stream of bits.
data MovingAverage = MovingAverage
  { -- | The list of on and off bits
    _bits :: [Bit],
    -- | The window size of the moving average
    _window :: Natural
  }
  deriving (Show, Generic)

makeLenses ''MovingAverage


-- -------------------------------------------------------------
--                           VIEW
-- -------------------------------------------------------------
--instance Show MovingAverage where
--show = show . _bits


-- -------------------------------------------------------------
--                           UPDATES
-- -------------------------------------------------------------

-- | Prepend a bit to the moving average and caps the size of the list to '_window'.
prepend :: Bit -> MovingAverage -> MovingAverage
prepend bit mva = mva & bits %~ move bit (mva ^. window)

-- | Prepends a value to a list of bits, and caps the size of the list to maximum size.
move :: Bit -> Natural -> [Bit] -> [Bit]
move bit max bits = if intToNatural (length bits) >= max then init (bit : bits) else bit : bits

-- | Sums the on bits in a list of bits.
sumBits :: [Bit] -> Natural
sumBits = foldr ((+) . intToNatural . fromEnum) 0

-- | Prepend an on bit to the moving-average
--
-- >>> on MovingAverage{_bits=[], _window=2}
-- MovingAverage {_bits = [True], _window = 2}
on :: MovingAverage -> MovingAverage
on = prepend True

-- | Prepend an off bit to the moving-average
--
-- >>> off MovingAverage{_bits=[], _window=2}
-- MovingAverage {_bits = [False], _window = 2}
off :: MovingAverage -> MovingAverage
off = prepend False


-- -------------------------------------------------------------
--                          STATISTICS
-- -------------------------------------------------------------

-- | Computes the percentage of on bits within the '_window' size.
--
-- prop> = nr of on bits / _window
-- 
-- >>> sparsity $ MovingAverage{_bits=[False, True, True], _window=5}
-- 0.4
sparsity :: MovingAverage -> Float
sparsity mva =  if w == 0 then 0 else c / w
  where
    c = fromIntegral (sumBits (mva ^. bits))
    w = fromIntegral (mva ^. window)

-- | Computes the average of the moving average.
-- It only computes from the values already prepended and does not care about the '_window' size.
--
-- prop> = nr of on bits / length of _bits
-- 
-- >>> average $ MovingAverage{_bits=[False, True, True], _window=5}
-- 0.6666667
average :: MovingAverage -> Float
average mv = if l == 0 then 0 else c / l
  where
    c = fromIntegral (sumBits (mv ^. bits))
    l = fromIntegral (length (mv ^. bits))
