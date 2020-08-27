module Main where
import           Data.Maybe
import           HaskellSay (haskellSay)
main :: IO ()
main = haskellSay "Hello, Haskell!"

-- Unused Code from HTM
{-
initSensorySynapses :: Int -> Config -> SDR -> [SensorySynapse]
initSensorySynapses index config
  | mappingSDRToRegion config == Random = map (\x -> (x, initConnectionStrength config)) $ selectRandom index config $ inputfield config --TODO choose random subset of inputfield portal choose init connection strength

-- select Random pixels of the sensory inputField
selectRandom :: Int -> Config -> SDRRange -> [BitIndex]
selectRandom index config ls = randElements (potentialNrOfSensorySynapses config) index ls

randElements :: Int -> Int -> [a] -> [a]
randElements n index ls
  | n < 0 = []
  | n == 0 = []
  | n > 0 = randElem index ls : randElements (n-1) index ls

randElem :: Int -> [a] -> a -- does not work for empty list
randElem index ls =  let g = mkStdGen index in
                          (\l g -> l !! fst (randomR (0, length l) g)) ls g

slidingWindow index ls = drop (index) $ take (index + 10) ls
wrappedSlidingWindow index ls = drop (index) $ take (index + 10) ls

-}
