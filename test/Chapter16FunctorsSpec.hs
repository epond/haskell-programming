module Chapter16FunctorsSpec where

import Test.Hspec
import Test.QuickCheck
import Chapter16Functors

spec :: Spec
spec = do
    describe "QuickCheck" $ do
        it "can check the Functor identity law" $ do
            property (functorIdentity :: [Int] -> Bool)
        it "can check the Functor composition law" $ do
            property (\x -> (functorCompose (+1) (*2)) (x :: [Int]))
    describe "Exercises: Instances of Func" $ do
        it "has a Functor instance for Identity that satisfies identity law" $ do
            property (functorIdentity :: Identity Int -> Bool)
        it "has a Functor instance for Identity that satisfies composition law" $ do
            property (\x -> (functorCompose (+1) (*2)) (x :: Identity Int))
        it "has a Functor instance for Pair that satisfies identity law" $ do
            property (functorIdentity :: Pair Int -> Bool)
        it "has a Functor instance for Pair that satisfies composition law" $ do
            property (\x -> (functorCompose (+1) (*2)) (x :: Pair Int))
        it "has a Functor instance for Two that satisfies identity law" $ do
            property (functorIdentity :: Two Int Bool -> Bool)
        it "has a Functor instance for Two that satisfies composition law" $ do
            property (\x -> (functorCompose (+1) (*2)) (x :: Two Bool Int))
        it "has a Functor instance for Three that satisfies identity law" $ do
            property (functorIdentity :: Three Int Bool Char -> Bool)
        it "has a Functor instance for Three that satisfies composition law" $ do
            property (\x -> (functorCompose (+1) (*2)) (x :: Three Bool Char Int))

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Pair x y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)