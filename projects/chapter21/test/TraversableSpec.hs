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
    it "Optional instance of Traversable" $ do
      hspec $ testBatch (traversable (undefined :: Optional (Int, Int, [Int])))
    it "List instance of Traversable" $ do
      hspec $ testBatch (traversable (undefined :: List (Int, Int, [Int])))
    it "Three instance of Traversable" $ do
      hspec $ testBatch (traversable (undefined :: Three Bool String (Int, Int, [Int])))
    it "Pair instance of Traversable" $ do
      hspec $ testBatch (traversable (undefined :: Pair String (Int, Int, [Int])))
    it "Big instance of Traversable" $ do
      hspec $ testBatch (traversable (undefined :: Big String (Int, Int, [Int])))

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

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    frequency [ (1, return $ Yep x),
                (1, return Nada) ]

instance Eq a => EqProp (Optional a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return Nil)
                  , (1, return $ Cons x Nil)
                  , (1, return $ Cons x $ Cons y Nil) ]

instance Eq a => EqProp (List a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq
      
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq
      
instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Big x y z)

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq
      
-- | Allows to insert a 'TestBatch' into a Spec. (code taken from hspec-checkers library)
testBatch :: TestBatch -> Spec
testBatch (batchName, tests) = describe ("laws for: " ++ batchName) $
    foldr (>>) (return ()) (map (uncurry it) tests)
