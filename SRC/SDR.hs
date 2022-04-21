{-# LANGUAGE TemplateHaskell, DeriveGeneric, NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}


--{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

-- |
-- Module      : SRC.SDR
-- Description : A record representing an SDR
-- Copyright   : (c) Fromsa Hera, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module represents an 'SDR' as defined by (Numenta).
-- See [Encoding Data for HTM Systems](https://arxiv.org/pdf/1602.05925.pdf) for detailed explanation.
module SRC.SDR where

import Control.Lens (makeLenses)
import SRC.CommonDataTypes (BitIndex)
import GHC.Generics ( Generic ) 
import Diagrams.Prelude 
import Diagrams.Backend.SVG
import Diagrams.Size

-- | A data type for the possible range of an SDR value.
data SDRRange = SDRRange
  { -- | The smallest sdr bit index. Implisitly = 0
    _minIndex :: BitIndex,
    -- | The largest sdr bit index.
    _maxIndex :: BitIndex
  }
  deriving (Show, Generic)

makeLenses ''SDRRange

-- | The range of an SDR.
data SDR = SDR
  { -- | A list of bit indecies. Used as a representaion of an input value, like an integer, class or other data type.
    --  The values are transformed to SDR using an encoder
    _sdr :: [BitIndex],
    _sdrRange :: SDRRange
  }
  deriving (Show)

makeLenses ''SDR

visualize :: SDR -> Diagram B
visualize s = let range = s^.sdrRange in 
                let as = replicate (fromIntegral ((range^.maxIndex) - (range^.minIndex))) 0 in
                  toDiagram $ rendder $ replace (s^.sdr) as 1

replace :: [BitIndex] -> [Int] -> Int -> [Int]
replace [] bits _ = bits
replace (b:bs) bits a = let (x,_:ys) = splitAt (fromIntegral b) bits in
                          replace bs (x++(a:ys)) a


toDiagram (x:xs) = foldl (|||) x xs

rendder [x] = [rend x]
rendder (x:xs) = rend x : rendder xs
 
rend 1 = activeCell 1
rend _ = inactiveCell 1

visualCell :: Double -> Colour Double -> Diagram B
visualCell x c = square x # fc c
                    # lw veryThick
                    # lc black

activeCell :: Double -> Diagram B
activeCell d = visualCell (d*0.9) black `atop` visualCell d white


inactiveCell :: Double -> Diagram B
inactiveCell d = visualCell d white
