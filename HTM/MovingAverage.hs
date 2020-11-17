{-# LANGUAGE TemplateHaskell #-}

module HTM.MovingAverage where

import Control.Lens ( (&), (^.), (%~), makeLenses )
import GHC.Natural ( Natural, intToNatural )

type Bit = Bool

-- MovingAverage
data MovingAverage = MovingAverage
  { -- | The list of on and of bits
    _bits :: [Bit],
    -- | The window size of the moving average
    _window :: Natural
  }

makeLenses ''MovingAverage

instance Show MovingAverage where
  show = show . _bits

append :: Bit -> MovingAverage -> MovingAverage
append bit mva = mva & bits %~ move bit (mva ^. window)

move :: Bit -> Natural -> [Bit] -> [Bit]
move bit max bits = if intToNatural (length bits) >= max then init (bit : bits) else bit : bits

average :: MovingAverage -> Float
average mv = fromIntegral (sumBits (mv ^. bits)) / fromIntegral (length (mv ^. bits))

sumBits :: [Bit] -> Natural
sumBits = foldr ((+) . intToNatural . fromEnum) 0

-- | Appends an on bit to the movingaverage
on :: MovingAverage -> MovingAverage
on = append True

-- | append a 0 to MovingAverage
off :: MovingAverage -> MovingAverage
off = append False

averagePercent :: MovingAverage -> Float
averagePercent mva = average mva / fromIntegral (mva ^. window)