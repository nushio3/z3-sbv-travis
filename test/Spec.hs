import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad.IO.Class
import Data.SBV

import Test.QuickCheck
import Test.HUnit.Lang

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Sorting Group 1" [
                testProperty "logic is correct" prop1,
                testProperty "the answer is not reachable by QuickCheck" prop2,
                testCase "Z3 can find the answer" assert3
           ]
      ]

prop1 b = b || True
  where types = (b :: Bool)

prop2 i = i /= 42
  where types = (i :: Int)

assert3 :: Assertion
assert3 = do
  result <- prove $ \x y -> x*y .== (y*x::SInteger)
  if (not $ modelExists result)
    then return()
    else assertFailure $ show result
