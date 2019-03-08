module MonadsSpec where

import Monads
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

spec :: Spec
spec = do
  describe "18.7 Chapter Exercises part 1" $ do
    it "Nope obeys the Monad laws" $ do
      hspec $ testBatch (monad (undefined :: Nope (String, String, Int)))
    it "PhhhbbtttEither obeys the Monad laws" $ do
      hspec $ testBatch (monad (undefined :: PhhhbbtttEither String (String, String, Int)))
    it "Identity obeys the Monad laws" $ do
      hspec $ testBatch (monad (undefined :: Identity (String, String, Int)))
    it "List obeys the Monad laws" $ do
      hspec $ testBatch (monad (undefined :: List (String, String, Int)))
  describe "18.7 Chapter Exercises part 2" $ do
    it "j function behaves as expected" $ do
      j [[1, 2], [], [3]] `shouldBe` [1, 2, 3]
      j (Just (Just 1)) `shouldBe` Just 1
    it "meh function behaves as expected" $ do
      meh [1,3] (\x -> Just (x*2)) `shouldBe` Just [1,6]
      
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

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return Nil)
                  , (1, return $ Cons x Nil)
                  , (1, return $ Cons x $ Cons y Nil) ]

instance Eq a => EqProp (List a) where (=-=) = eq

-- | Allows to insert a 'TestBatch' into a Spec. (code taken from hspec-checkers library)
testBatch :: TestBatch -> Spec
testBatch (batchName, tests) = describe ("laws for: " ++ batchName) $
    foldr (>>) (return ()) (map (uncurry it) tests)
