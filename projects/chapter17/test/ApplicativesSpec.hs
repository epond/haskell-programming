module ApplicativesSpec where

import Control.Applicative
import Applicatives
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

spec :: Spec
spec = do
    describe "List Applicative" $ do
        it "gives the expected result" $ do
            let f = Cons (+1) (Cons (*2) Nil)
            let v = Cons 1 (Cons 2 Nil)
            f <*> v `shouldBe` Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
        it "obeys the Applicative laws" $ do
            hspec $ testBatch (applicative (undefined :: List (String, String, Int)))
    describe "ZipList Applicative" $ do
        it "gives the expected result with a finite list" $ do
            let z  = ZipList' $ Cons (+9) (Cons (*2) (Cons (+8) Nil))
            let z' = ZipList' $ Cons 1 (Cons 2 (Cons 3 Nil))
            z <*> z' `shouldBe` (ZipList' (Cons 10 (Cons 4 (Cons 11 Nil))))
        it "gives the expected result with an infinite list" $ do
            let z  = ZipList' $ Cons (+9) (Cons (*2) (Cons (+8) Nil))
            let z' = ZipList' $ repeat' 1
            z <*> z' `shouldBe` (ZipList' (Cons 10 (Cons 2 (Cons 9 Nil))))
        it "obeys the Applicative laws" $ do
            hspec $ testBatch (applicative (undefined :: ZipList' (String, String, Int)))
--     describe "built-in ZipList Applicative" $ do
--         it "gives the expected result" $ do
--             let z  = ZipList [(+9), (*2), (+8)]
--             let z' = ZipList [1..3]
--             z <*> z' `shouldBe` (ZipList [10, 4, 11])
--         it "obeys the Applicative laws" $ do
--             hspec $ testBatch (applicative (undefined :: ZipList (String, String, Int)))
    describe "Validation Functor" $ do
        it "obeys the Functor laws" $ do
            hspec $ testBatch (functor (undefined :: Validation String (String, String, Int)))
    describe "Validation Applicative" $ do
        it "gives the expected result when two successes" $ do
            Succ (+1) <*> Succ 1 `shouldBe` (Succ 2 :: Validation [Errors] Int)
        it "gives the expected result when a failure on the right" $ do
            Succ (+1) <*> Fail [StackOverflow] `shouldBe` (Fail [StackOverflow] :: Validation [Errors] Int)
        it "gives the expected result when a failure on the left" $ do
            Fail [StackOverflow] <*> Succ 1 `shouldBe` (Fail [StackOverflow] :: Validation [Errors] Int)
        it "gives the expected result when both arguments are failures" $ do
            Fail [MooglesChewedWires] <*> Fail [StackOverflow] `shouldBe` (Fail [MooglesChewedWires, StackOverflow] :: Validation [Errors] Int)
        it "obeys the Applicative laws" $ do
            hspec $ testBatch (applicative (undefined :: Validation String (String, String, Int)))
    describe "17.9 Chapter Exercises" $ do
        it "Pair obeys the Applicative laws" $ do
            hspec $ testBatch (applicative (undefined :: Pair (String, String, Int)))
        it "Two obeys the Applicative laws" $ do
            hspec $ testBatch (applicative (undefined :: Two String (String, String, Int)))
        it "Three obeys the Applicative laws" $ do
            hspec $ testBatch (applicative (undefined :: Three String String (String, String, Int)))
        it "Three' obeys the Applicative laws" $ do
            hspec $ testBatch (applicative (undefined :: Three' String (String, String, Int)))

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return Nil)
                  , (1, return $ Cons x Nil)
                  , (1, return $ Cons x $ Cons y Nil) ]

instance Eq a => EqProp (List a) where (=-=) = eq

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return $ ZipList' Nil)
                  , (1, return $ ZipList' (Cons x Nil))
                  , (1, return $ ZipList' (Cons x $ Cons y Nil)) ]

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take' 300 l
              ys' = let (ZipList' l) = ys
                    in take' 300 l

-- instance Eq a => EqProp (ZipList a) where
--     xs =-= ys = xs' `eq` ys'
--         where xs' = let (ZipList l) = xs
--                     in take 3000 l
--               ys' = let (ZipList l) = ys
--                     in take 3000 l

-- | Allows to insert a 'TestBatch' into a Spec. (code taken from hspec-checkers library)
testBatch :: TestBatch -> Spec
testBatch (batchName, tests) = describe ("laws for: " ++ batchName) $
    foldr (>>) (return ()) (map (uncurry it) tests)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return $ Fail x)
                  , (1, return $ Succ y)]

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

data Errors = DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Pair x y)

instance Eq a => EqProp (Pair a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three' x y z)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq
