import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad.IO.Class
import Data.SBV

import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Sorting Group 1" [
                testProperty "logic is correct" prop1,
                testProperty "the answer is not reachable by QuickCheck" prop2,
                testProperty "Z3 can find the answer" prop3
           ]
      ]

prop1 b = b || True
  where types = (b :: Bool)

prop2 i = i /= 42
  where types = (i :: Int)

prop3 :: Property
prop3 = monadicIO $ do
  result <- liftIO $ prove $ \x y -> x*y .== (y*x::SInteger)
  -- in case of proof, assert non-existence of counterexample
  assert $ not $ modelExists result
