{-# LANGUAGE TemplateHaskell #-}

module Testing.Encoder where

import GHC.Natural
import Generic.Random
import HTM.Encoder.Numeric
import HTM.SDR
import System.Random
import Test.QuickCheck
import Test.QuickCheck.All ()
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances ()

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
chooseNatural (a,b) = MkGen (\r _ -> let (x, _) = randomR (a,b) r in intToNatural (max (max 0 a) x))

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


--------------------------------------
--           Run All tests          --
return []

runTests :: IO Bool
runTests = $quickCheckAll

l = quickCheck prop_MaxValIsLargerOrEqualMinVal

g = generate (genericArbitrarySingle :: Gen EncoderConfig)
