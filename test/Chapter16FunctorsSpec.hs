{-# LANGUAGE FlexibleInstances #-}

module Chapter16FunctorsSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
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
            property composeIdentity
        it "has a Functor instance for Pair that satisfies identity law" $ do
            property (functorIdentity :: Pair Int -> Bool)
        it "has a Functor instance for Pair that satisfies composition law" $ do
            property composePair
        it "has a Functor instance for Two that satisfies identity law" $ do
            property (functorIdentity :: Two Int Bool -> Bool)
        it "has a Functor instance for Two that satisfies composition law" $ do
            property composeTwo
        it "has a Functor instance for Three that satisfies identity law" $ do
            property (functorIdentity :: Three Int Bool Char -> Bool)
        it "has a Functor instance for Three that satisfies composition law" $ do
            property composeThree
        it "has a Functor instance for Three' that satisfies identity law" $ do
            property (functorIdentity :: Three' Bool Int -> Bool)
        it "has a Functor instance for Three' that satisfies composition law" $ do
            property composeThree'
        it "has a Functor instance for Four that satisfies identity law" $ do
            property (functorIdentity :: Four Int Bool Char Int -> Bool)
        it "has a Functor instance for Four that satisfies composition law" $ do
            property composeFour
        it "has a Functor instance for Four' that satisfies identity law" $ do
            property (functorIdentity :: Four' Int Bool Char -> Bool)
        it "has a Functor instance for Four' that satisfies composition law" $ do
            property composeFour'
    describe("16.11 Ignoring Possibilities. Exercise: Possibly") $ do
        it "has a Functor instance that satisfies identity law" $ do
            property (functorIdentity :: Possibly Int -> Bool)
        it "has a Functor instance that satisfies composition law" $ do
            property composePossibly
    describe("Short Exercise: Sum") $ do
        it "has a Functor instance that satisfies identity law" $ do
            property (functorIdentity :: Sum Bool Int -> Bool)
        it "has a Functor instance that satisfies composition law" $ do
            property composeSum
    describe("16.17 Write Functor instances for these datatypes") $ do
        it "Quant has a Functor instance that satisfies identity law" $ do
            property (functorIdentity :: Quant Bool Int -> Bool)
        it "Quant has a Functor instance that satisfies composition law" $ do
            property composeQuant
        it "K has a Functor instance that satisfies identity law" $ do
            property (functorIdentity :: K Bool Int -> Bool)
        it "K has a Functor instance that satisfies composition law" $ do
            property composeK
        it "Flip K' has a Functor instance that satisfies identity law" $ do
            property (functorIdentity :: (Flip K' Bool Int) -> Bool)
        it "Flip K' has a Functor instance that satisfies composition law" $ do
            property composeFlipK

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

composeIdentity :: Identity Int -> Fun Int Int -> Fun Int Int -> Bool
composeIdentity x (Fun _ f) (Fun _ g) = functorCompose f g x

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Pair x y)

composePair :: Pair Int -> Fun Int Int -> Fun Int Int -> Bool
composePair x (Fun _ f) (Fun _ g) = functorCompose f g x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

composeTwo :: Two Bool Int -> Fun Int Int -> Fun Int Int -> Bool
composeTwo x (Fun _ f) (Fun _ g) = functorCompose f g x

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)

composeThree :: Three Bool Char Int -> Fun Int Int -> Fun Int Int -> Bool
composeThree x (Fun _ f) (Fun _ g) = functorCompose f g x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three' x y z)

composeThree' :: Three' Bool Int -> Fun Int Int -> Fun Int Int -> Bool
composeThree' x (Fun _ f) (Fun _ g) = functorCompose f g x

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Four w x y z)

composeFour :: Four Bool Int Char Int -> Fun Int Int -> Fun Int Int -> Bool
composeFour x (Fun _ f) (Fun _ g) = functorCompose f g x

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Four' a b c) where
    arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Four' w x y z)

composeFour' :: Four' Bool Char Int -> Fun Int Int -> Fun Int Int -> Bool
composeFour' x (Fun _ f) (Fun _ g) = functorCompose f g x


instance Arbitrary a => Arbitrary (Possibly a) where
    arbitrary = do
        x <- arbitrary
        frequency [ (1, return LolNope)
                  , (1, return $ Yeppers x) ]

composePossibly :: Possibly Int -> Fun Int Int -> Fun Int Int -> Bool
composePossibly x (Fun _ f) (Fun _ g) = functorCompose f g x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return $ First x)
                  , (1, return $ Second y) ]

composeSum :: Sum Bool Int -> Fun Int Int -> Fun Int Int -> Bool
composeSum x (Fun _ f) (Fun _ g) = functorCompose f g x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return Finance)
                  , (1, return $ Desk x)
                  , (1, return $ Bloor y) ]

composeQuant :: Quant Bool Int -> Fun Int Int -> Fun Int Int -> Bool
composeQuant x (Fun _ f) (Fun _ g) = functorCompose f g x

instance Arbitrary a => Arbitrary (K a b) where
    arbitrary = do
        x <- arbitrary
        return (K x)

composeK :: K Bool Int -> Fun Int Int -> Fun Int Int -> Bool
composeK x (Fun _ f) (Fun _ g) = functorCompose f g x

instance Arbitrary b => Arbitrary (Flip K' a b) where
    arbitrary = do
        x <- arbitrary
        return $ Flip (K' x)

composeFlipK :: (Flip K' Bool) Int -> Fun Int Int -> Fun Int Int -> Bool
composeFlipK x (Fun _ f) (Fun _ g) = functorCompose f g x
