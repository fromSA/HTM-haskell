--{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveGeneric, NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, OverloadedStrings #-}

module SRC.MAIN where

import Data.List.Split (chunksOf, splitPlaces)
import Data.List (unfoldr, maximum, elemIndex)

import GHC.Generics
import Control.Applicative ((<|>))
import Data.Either.Extra (mapLeft)
import Control.Concurrent.MVar
import Control.Lens ((^.),makeLenses)
import GHC.Natural (intToNatural, naturalToInt)
import Debug.Trace (trace)

import SRC.SDR
import SRC.Package
import SRC.CommonDataTypes
import REPL
import SRC.HTM.Config
import SRC.HTM.HTM

splitIntoBuckets :: Int -> Int -> [Int]
splitIntoBuckets max k = 
    let (bucketSize, remainder) = max `divMod` k
        sizes = replicate remainder (bucketSize + 1) ++ replicate (k - remainder) bucketSize
    in  init $ tail $ scanl (+) 0 sizes

splitRangeIntoSubList :: BitIndex -> SDRRange -> Maybe [BitIndex]
splitRangeIntoSubList n range = let size = range^.maxIndex - range^.minIndex
                                    step =  size `div` n
                                in if size > n
                                    then Just $ map intToNatural $ splitIntoBuckets (naturalToInt size) (naturalToInt n) --enumFromThenTo (range^.minIndex) step (range^.maxIndex)
                                    else Nothing




splitAtPositions :: [BitIndex] -> [BitIndex] -> [Int]
splitAtPositions xs [] = if null xs
      then [0]
      else [length xs]
splitAtPositions xs (y:ys) = 
    let (front, back) = span (< y) xs
    in if null front
      then 0 : splitAtPositions back ys
      else length front : splitAtPositions back ys



indexOfMax :: Ord a => [a] -> Maybe BitIndex
indexOfMax xs = do 
                x <- elemIndex (maximum xs) xs 
                Just $ intToNatural x


fromSDR ::  BitIndex -> SDR -> Maybe BitIndex
fromSDR decoder s = let a = splitRangeIntoSubList decoder (s^.sdrRange)
                        in  case a of 
                                Nothing -> Nothing
                                Just a -> indexOfMax $ tracer "count is " $ splitAtPositions (trace ("[ TRACE ] --- sdr is: " ++ show (s^.sdr))(s^.sdr)) (trace ("[ TRACE ] --- split is " ++ show a) a)


tracer :: (Show a) => String -> a -> a
tracer s v = trace ("[ TRACE ] --- " ++ s ++ show v) v

m :: IO ()
m = do
  let sdr = SDR [0..2] $ SDRRange 0 10 -- the value can be 0 or 10 =  meaning the range is 11. -> How do we split odd values? append it to the last cell?
  case fromSDR 2 sdr of
    Nothing -> print ""
    Just v -> print $ tracer "value is: " v



m2 max k = do
  -- range = 12
  --print $ enumFromThenTo 0 5 8 -- start step max
  --print $ start max groups
  -- first we take step = floor $ (max-start)/group 
  -- and the remains = (max-start) % group
  --let max = 12
  --let k = 2
  let l = max `mod` k
  let n = max `div` k
  --print $ splitPoints n k l 
  print $ splitIntoBuckets max k 


