{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module TEST.Encoder where

import GHC.Generics (Generic)
import GHC.Natural
import Generic.Random
import SRC.Encoder.Numeric
import SRC.SDR
import System.Random
import qualified Test.HUnit as TH
import Test.QuickCheck
import Test.QuickCheck.All ()
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances ()
import Text.Printf
import SRC.Encoder.Config

-------------------------------------
--           Arbitraries           --

instance Arbitrary EncoderType where
  arbitrary = chooseEnum (Numeric, Categorical)

instance Arbitrary EncoderConfig where
  arbitrary = do
    ty <- arbitrary :: Gen EncoderType
    mi <- arbitrary :: Gen Int
    ma <- choose (mi, mi + 1000) -- This generates min and max with difference of maximum 1000
    bu <- chooseNatural (1, mi - ma) :: Gen Natural
    bi <- chooseNatural (1, 100) :: Gen Natural -- This generates natural number between 1 and 100
    return $ EncoderConfig ty mi ma bu bi

chooseNatural :: (Int, Int) -> Gen Natural
chooseNatural (a, b) = MkGen (\r _ -> let (x, _) = randomR (a, b) r in intToNatural (max (max 0 a) x))


data ValidInt = ValidInt {v :: Int, c :: EncoderConfig} deriving (Show, Generic)

instance Arbitrary ValidInt where
  arbitrary = do
    c <- arbitrary :: Gen EncoderConfig
    a <- chooseInt (_minVal c, _maxVal c)
    return $ ValidInt a c


data InValidInt = InValidInt {iv :: Int, ic :: EncoderConfig} deriving (Show, Generic)

instance Arbitrary InValidInt where
  arbitrary = do
    c <- arbitrary :: Gen EncoderConfig
    a <- chooseInt (_minVal c - 100, _minVal c - 1)
    return $ InValidInt a c

-------------------------------------
--           Properties            --

-- EncoderConfig
prop_MaxValIsLargerOrEqualMinVal :: EncoderConfig -> Bool
prop_MaxValIsLargerOrEqualMinVal = checkEncoderInvariant

-- getRange
checkSDRRangeInvariant :: SDRRange -> Bool
checkSDRRangeInvariant r =
  _minIndex r >= 0
    && _maxIndex r >= 0
    && _maxIndex r >= _minIndex r

prop_getRange :: EncoderConfig -> Bool
prop_getRange r = maybe False checkSDRRangeInvariant (getRange r)

-- encode
prop_encodeValidInput :: ValidInt -> Bool
prop_encodeValidInput inn = case encode (c inn) (v inn) of
  Just _ -> True
  Nothing -> False

prop_rejectInValidInput :: InValidInt -> Bool
prop_rejectInValidInput inn = case encode (ic inn) (iv inn) of
  Just _ -> False
  Nothing -> True

checkSDREncodingInvariant :: SDR -> Bool
checkSDREncodingInvariant s =
  a
    && head (x : xs) >= _minIndex (_sdrRange s) -- with in SDR Range
    && last (x : xs) <= _maxIndex (_sdrRange s) -- with in SDR Range
  where
    (x : xs) = _sdr s
    a = length (x : xs) <= 1 || fst (foldl (\(b, prev) next -> (b && (prev + 1) == next, next)) (True, x) xs) -- continues indecies

prop_encodedSDRInvariant :: ValidInt -> Bool
prop_encodedSDRInvariant inn = maybe False checkSDREncodingInvariant (encode (c inn) (v inn))

-------------------------------------
--           UnitTests             --

generateTestList :: ((a, Int) -> TH.Test) -> [a] -> [TH.Test]
generateTestList f xs = fmap f (zip xs [0 ..])

-- getStartOf <- computeStart

startCases :: [(Natural, (Int, Int, Int, Int))]
startCases =
  [ (0, (-1, 1, -1, 1)),
    (0, (0, 1, -1, 1)),
    (0, (1, 1, -1, 1)),
    (0, (-1, 2, -1, 1)),
    (0, (0, 2, -1, 1)),
    (1, (1, 2, -1, 1)),
    (0, (0, 1, 0, 0)),
    (0, (1, 2, 0, 0)),
    (0, (1, 1, 0, 1)),
    (0, (0, 2, 0, 1)),
    (0, (1, 1, 0, 1)),
    (1, (1, 2, 0, 1)),
    (1, (1, 3, 0, 1))
  ]

---- TestCases
generateTest :: ((Natural, (Int, Int, Int, Int)), Int) -> TH.Test
generateTest ((t, (p1, p2, p3, p4)), casei) = TH.TestLabel testName test
  where
    dscp = printf "For computeStart %d %d %d %d," p1 p2 p3 p4
    test = TH.TestCase (TH.assertEqual dscp t (computeStart p1 p2 p3 p4))
    testName = printf "startTest%d" casei

---- Testsuit
startTestList :: [(Natural, (Int, Int, Int, Int))] -> [TH.Test]
startTestList = generateTestList generateTest

-- totNrOfBits <- computeTotalNumberOfBits
casesTNB :: [(Natural, (Natural, Natural))]
casesTNB =
  [ (1, (1,1)),
    (2, (1,2)),
    (2, (2,1)),
    (3,(1,3)),
    (4,(2,3)),
    (5,(3,3))
  ]

---- TestCases
generateTestTNB :: ((Natural, (Natural, Natural)), Int) -> TH.Test
generateTestTNB ((t, (p1, p2)), casei) = TH.TestLabel testName test
  where
    dscp = printf "For computeTotalNumberOfBits %d %d," p1 p2
    test = TH.TestCase (TH.assertEqual dscp t (computeTotalNumberOfBits p1 p2))
    testName = printf "TNBTest%d" casei

---- Testsuit
tnbTestList :: [(Natural, (Natural, Natural))] -> [TH.Test]
tnbTestList = generateTestList generateTestTNB


-- All Unit tests
tests :: TH.Test
tests = TH.TestList (startTestList startCases ++ tnbTestList casesTNB)

--------------------------------------
--           Run All tests          --

---- Property based tests
return [] -- Yikes!

runTests :: IO Bool
runTests = $quickCheckAll

---- Unit tests
runUnits :: IO TH.Counts
runUnits = TH.runTestTT tests

l = quickCheck prop_MaxValIsLargerOrEqualMinVal

g = generate (genericArbitrarySingle :: Gen EncoderConfig)
