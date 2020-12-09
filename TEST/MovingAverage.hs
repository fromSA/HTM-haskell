{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module TEST.MovingAverage where

import GHC.Generics (Generic)
import GHC.Natural (Natural, naturalToInt)
import SRC.MovingAverage
import qualified Test.HUnit as TH
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Text.Printf (printf)

-------------------------------------
--           Invariants            --

{-
length bits <= window
-}

chinMovingAverage :: MovingAverage -> Bool
chinMovingAverage m = length (_bits m) <= naturalToInt (_window m)

-------------------------------------
--           Functions             --
{-

prepend
move
sumBits
on
off

averagePercent
average

-}

data UpdateMovingAverage = UpdateMovingAverage
  { count :: Natural,
    m :: MovingAverage
  }
  deriving (Show, Generic)

instance Arbitrary MovingAverage where
  arbitrary = MovingAverage [] <$> arbitrary

instance Arbitrary UpdateMovingAverage where
  arbitrary = do
    i <- arbitrary :: Gen Natural
    m <- arbitrary :: Gen MovingAverage
    return $ UpdateMovingAverage i m

-------------------------------------
--           Properties            --

prop_On :: UpdateMovingAverage -> Bool
prop_On ma
  | n == 0 = chinMovingAverage (m ma)
  | otherwise = prop_On nm
  where
    n = count ma
    nm = ma {count = n - 1, m = on (m ma)}

prop_Off :: UpdateMovingAverage -> Bool
prop_Off ma
  | n == 0 = chinMovingAverage (m ma)
  | otherwise = prop_On nm
  where
    n = count ma
    nm = ma {count = n -1, m = off (m ma)}

-------------------------------------
--           Unit Tests            --

generateTestList :: ((a, Int) -> TH.Test) -> [a] -> [TH.Test]
generateTestList f xs = fmap f (zip xs [0 ..])

generateTest :: ((Float, MovingAverage), Int) -> String -> String -> (MovingAverage -> Float) -> TH.Test
generateTest ((expected, m), casei) dscp testName f = TH.TestLabel testName' test
  where
    dscp' = dscp ++ show m
    test = TH.TestCase (TH.assertEqual dscp' expected (f m))
    testName' = testName ++ show casei

--           Percentage
-------------------------------------
percentCases :: [(Float, MovingAverage)]
percentCases =
  [ (0, MovingAverage [] 0),
    (0, MovingAverage [] 1),
    (1, MovingAverage [True] 1),
    (0, MovingAverage [False] 1),
    (1 / 2, MovingAverage [False, True] 2),
    (2 / 2, MovingAverage [True, True] 2),
    (1 / 3, MovingAverage [False, True] 3),
    (1 / 3, MovingAverage [False, True, False] 3)
  ]

---- TestCases
generatePercentTest :: ((Float, MovingAverage), Int) -> TH.Test
generatePercentTest a = generateTest a "For averagePercent of " "percentageTest" averagePercent

---- Testsuit
percentTestList :: [(Float, MovingAverage)] -> [TH.Test]
percentTestList = generateTestList generatePercentTest

--           Average
-------------------------------------
averageCases :: [(Float, MovingAverage)]
averageCases =
  [ (0, MovingAverage [] 0),
    (0, MovingAverage [] 1),
    (1, MovingAverage [True] 1),
    (0, MovingAverage [False] 1),
    (1 / 2, MovingAverage [False, True] 2),
    (1, MovingAverage [True, True] 2),
    (1 / 2, MovingAverage [False, True] 3),
    (1 / 3, MovingAverage [False, True, False] 3)
  ]

---- TestCases
generateAverageTest :: ((Float, MovingAverage), Int) -> TH.Test
generateAverageTest a = generateTest a "For average of " "averageTest" average

---- Testsuit
averageTestList :: [(Float, MovingAverage)] -> [TH.Test]
averageTestList = generateTestList generateAverageTest

-- All Unit tests
tests :: TH.Test
tests = TH.TestList (percentTestList percentCases ++ averageTestList averageCases)

--------------------------------------
--           Run All tests          --

---- Property based tests
return [] -- Yikes!

runTests :: IO Bool
runTests = $quickCheckAll

---- Unit tests
runUnits :: IO TH.Counts
runUnits = TH.runTestTT tests