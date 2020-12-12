{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : LightRegion
Description : Short description
Copyright   : (c) Fromsa Hera, 2020
                  Numenta, 2020
License     : GPL-3
Maintainer  : fromsahera28@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Region where

import           MovingAverage 
import           System.Random
import           Data.List     (intercalate)
import SDR(BitIndex(..), SDRRange(..), SDRConfig(..))
import Control.Lens hiding (element)



-- -------------------------------------------------------------
--                        Model
-- -------------------------------------------------------------


-- |The values that encode the difference between current timestep columns and previous timestep columns
data ColumnTimeDiff = Step{ 
   _columnState :: ColumnState
  , _cells       :: CellDiff 
}
-- |The values that encode the difference between current timestep and previous timestep of the same cell
data CellDiff = CellStep{
  _dendrites :: [Dendrite] -- ^A set of dendrites. 
  , _cellState :: CellState -- ^The state of this cell.
  , _isWinner :: Bool
}

------ Lenses

makeLenses ''ColumnTimeDiff
makeLenses ''CellDiff
