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
                    in take' 3000 l
              ys' = let (ZipList' l) = ys
                    in take' 3000 l

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
