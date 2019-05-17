module TraversableSpec where

import Traversable
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

spec :: Spec
spec = do
  describe "21.12 Chapter Exercises" $ do
    it "Identity instance of Traversable" $ do
      hspec $ testBatch (traversable (undefined :: Identity (Int, Int, [Int])))
    it "Constant instance of Traversable" $ do
      hspec $ testBatch (traversable (undefined :: Constant String (Int, Int, [Int])))

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Arbitrary b => Arbitrary (Constant b a) where
  arbitrary = do
    x <- arbitrary
    return (Constant x)

instance (Eq a, Eq b) => EqProp (Constant b a) where (=-=) = eq

-- | Allows to insert a 'TestBatch' into a Spec. (code taken from hspec-checkers library)
testBatch :: TestBatch -> Spec
testBatch (batchName, tests) = describe ("laws for: " ++ batchName) $
    foldr (>>) (return ()) (map (uncurry it) tests)
