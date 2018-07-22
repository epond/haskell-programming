module ApplicativesSpec where

import Applicatives
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

spec :: Spec
spec = do
    describe "List Applicative" $ do
        it "obeys the Applicative laws" $ do
            -- TODO This does not fail the test when the checkers checks fail!!!
            hspec $ testBatch (applicative (undefined :: List (String, String, Int)))

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