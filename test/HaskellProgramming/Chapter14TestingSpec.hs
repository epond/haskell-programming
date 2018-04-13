module HaskellProgramming.Chapter14TestingSpec where

import Test.Hspec
import Test.QuickCheck
import HaskellProgramming.Chapter14Testing
import Data.List (sort)

spec :: Spec
spec = do
    describe "Example-based test: My dividedBy function" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
    describe "Chapter Exercises: Using QuickCheck" $ do
        it "inline property: x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
        it "can check the half function" $ do
            property prop_halfIdentity
        it "can check that a sorted list Int is ordered" $ do
            property $ forAll (arbitrary :: Gen [Int]) (\x -> listOrdered (sort x))
        it "can check that plus is associative" $ do
            property $ forAll (genTuple3 :: Gen (Int, Int, Int)) plusAssociative
        it "can check that plus is commutative" $ do
            property $ forAll (genTuple2 :: Gen (Int, Int)) plusCommutative
        it "can check that multiplication is associative" $ do
            property $ forAll (genTuple3 :: Gen (Int, Int, Int)) multAssociative
        it "can check that multiplication is commutative" $ do
            property $ forAll (genTuple2 :: Gen (Int, Int)) multCommutative
        it "can check a relationship between quot and rem" $ do
            property $ forAll (genTuple2 :: Gen (Int, Positive Int)) quotRemLaw
        it "can check a relationship between div and mod" $ do
            property $ forAll (genTuple2 :: Gen (Int, Positive Int)) divModLaw
        it "can check that reversing a list twice ends up with the original list" $ do
            property $ forAll (arbitrary :: Gen String)
                (\x -> (reverse . reverse) x == x)
        -- Falsifiable (after 5 tests and 6 shrinks): 
        -- [0]
        -- [1]
        -- it "can check that foldr (:) == (++)" $ do
        --     property $ \x y -> foldr (:) (x :: [Int]) (y :: [Int]) == (++) x y
        it "can check that foldr (++) [] == concat" $ do
            property $ \x -> foldr (++) [] (x :: [[Int]]) == concat x


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)

half :: Fractional a => a -> a
half x = x / 2

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = (((*2) . half) x) == x

-- another way of saying the same thing but it shows where we could introduce a
-- generator for the Double type if we were not happy with the default
prop_halfIdentity' :: Property
prop_halfIdentity' =
    forAll (arbitrary :: Gen Double) (\x -> (((*2) . half) x) == x)

-- for any list you apply sort to this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

genTuple2 :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple2 = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genTuple3 :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genTuple3 = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

plusAssociative :: (Num a, Eq a) => (a, a, a) -> Bool
plusAssociative (x, y, z) = x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => (a, a) -> Bool
plusCommutative (x, y) = x + y == y + x

multAssociative :: (Num a, Eq a) => (a, a, a) -> Bool
multAssociative (x, y, z) = x * (y * z) == (x * y) * z

multCommutative :: (Num a, Eq a) => (a, a) -> Bool
multCommutative (x, y) = x * y == y * x

quotRemLaw :: Integral a => (a, Positive a) -> Bool
quotRemLaw (x, Positive y) = (quot x y) * y + (rem x y) == x

divModLaw :: Integral a => (a, Positive a) -> Bool
divModLaw (x, Positive y) = (div x y) * y + (mod x y) == x
