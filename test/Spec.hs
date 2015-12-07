import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.API (Test)

import Control.Monad.IO.Class
import Data.SBV

import Test.QuickCheck hiding ((==>))
import Test.HUnit.Lang

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Sorting Group 1" [
                testProperty "logic is correct" prop1,
                testProperty "the answer is not reachable by QuickCheck" prop2,
                testProof "Z3 can prove the cover" $ thmCover 100 100 ,
                testProof "Z3 can prove the too-powerful cover" $ thmCover 101 100 ,
                testDisproof "Z3 can disprove the false cover" $ thmCover 99 100,
                testProof "Z3 can prove the no overlap" $ thmNoOverlap 100 100,
                testProof "Z3 can prove the too-sparse no overlap" $ thmNoOverlap 99 100,
                testDisproof "Z3 can detect the overlap" $ thmNoOverlap 101 100
           ]
      ]

prop1 b = b || True
  where types = (b :: Bool)

prop2 i = i * i /= 42
  where types = (i :: Int)

testProof :: Provable a => String -> a -> Test
testProof msg thm = testCase msg $ do
  result <- prove thm
  if (not $ modelExists result)
    then return()
    else assertFailure $ show result

testDisproof :: Provable a => String -> a -> Test
testDisproof msg thm = testCase msg $ do
  result <- prove thm
  if (not $ modelExists result)
    then assertFailure $ show result
    else return ()


thmCover :: SInteger -> SInteger -> Predicate
thmCover n m = do
  x <- forall "x"
  i <- exists "i"
  j <- exists "j"
  return $ 0 .<= i &&& i .< n &&& j * m + i .==(x::SInteger)

thmNoOverlap :: SInteger -> SInteger -> Predicate
thmNoOverlap n m = do
  x <- forall "x"
  i <- forall "i"
  j <- forall "j"
  x' <- forall "x'"
  i' <- forall "i'"
  j' <- forall "j'"
  return $ 0 .<= i &&& i .< n &&& j * m + i .==x &&&
    0 .<= i' &&& i' .< n &&& j' * m + i' .==x'
    ==> ((i,j) .== (i', j') <=> x .== x')
