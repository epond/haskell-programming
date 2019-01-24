module MonadsSpec where

import Monads
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

spec :: Spec
spec = do
  describe "18.7 Chapter Exercises" $ do
    it "Nope obeys the Monad laws" $ do
      hspec $ testBatch (monad (undefined :: Nope (String, String, Int)))
    it "PhhhbbtttEither obeys the Monad laws" $ do
      hspec $ testBatch (monad (undefined :: PhhhbbtttEither String (String, String, Int)))
      
instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return (Monads.Left x))
              , (1, return (Monads.Right y))]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

-- | Allows to insert a 'TestBatch' into a Spec. (code taken from hspec-checkers library)
testBatch :: TestBatch -> Spec
testBatch (batchName, tests) = describe ("laws for: " ++ batchName) $
    foldr (>>) (return ()) (map (uncurry it) tests)
