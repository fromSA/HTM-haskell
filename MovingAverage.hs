{-# LANGUAGE TemplateHaskell #-}
module MovingAverage where

import Control.Lens hiding (element)

type Bit = Bool

-- MovingAverage 
data MovingAverage = MovingAverage {
    _bits      :: [Bit] -- ^The list of on and of bits
    , _window  :: Int -- ^The window size of the moving average
    } deriving (Show)

makeLenses ''MovingAverage

append :: Bit -> MovingAverage -> MovingAverage
append bit mva =  mva & bits %~ (move bit (mva^.window))

move :: Bit -> Int -> [Bit] -> [Bit] 
move bit max bits = if length bits >= max then init (bit:bits) else (bit:bits)

mapBoolInt :: Bool -> Int
mapBoolInt True = 1
mapBoolInt False = 0

average :: MovingAverage -> Float
average mv = fromIntegral (sumBits (mv^.bits)) / fromIntegral (length (mv^.bits))

sumBits :: [Bit] -> Int
sumBits =  foldr ((+).mapBoolInt) 0

-- |Appends an on bit to the movingaverage
on :: MovingAverage -> MovingAverage
on = append True 

-- |append a 0 to MovingAverage
off :: MovingAverage -> MovingAverage
off = append False

averagePercent :: MovingAverage -> Float
averagePercent mva = (average mva) / (fromIntegral (mva^.window))