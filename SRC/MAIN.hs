--{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveGeneric, NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, OverloadedStrings #-}

module SRC.MAIN where

import Data.List.Split (chunksOf, splitPlaces)
import Data.List (unfoldr, maximum, elemIndex)

import GHC.Generics
import Control.Applicative ((<|>))
import Data.Either.Extra (mapLeft)
import Control.Concurrent.MVar
import Control.Lens ((^.), makeLenses, itraversed, (<.), index, withIndex)
import Control.Lens.Fold ((^..))
import GHC.Natural (intToNatural, naturalToInt)
import Debug.Trace (trace)

import SRC.SDR
import SRC.Package
import SRC.CommonDataTypes
import REPL
import SRC.HTM.Config
import SRC.HTM.HTM


l :: [Int]
l = [1,2,3,4,5,5,6,7,7,23,2]


--main :: IO ()
--main = print $ l ^.. itraversed . withIndex . to length


-- Cell - dendrite (prox|dist) - segment - synapse - matchval
-- traverse cells -> get best dendrite
-- traverse prox -> get best segment
-- traverse dist -> get best dendrite
-- traverse dendrite -> get best segment
-- segment -> map to matching

