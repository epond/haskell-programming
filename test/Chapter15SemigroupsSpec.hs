module Chapter15SemigroupsSpec where

import Test.Hspec
import Test.QuickCheck
import Chapter15Semigroups
import Data.Semigroup

spec :: Spec
spec = do
    describe "The Semigroup instance for Trivial" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
    describe "The Semigroup instance for Identity" $ do
        it "satisfies the associativity law on Trivial" $ do
            property (semigroupAssoc :: Identity Trivial -> Identity Trivial -> Identity Trivial -> Bool)
        it "satisfies the associativity law on String" $ do
            property (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
    describe "The Semigroup instance for Two" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: Two String Bool -> Two String Bool -> Two String Bool -> Bool)
    describe "The Semigroup instance for Three" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: Three String String String -> Three String String String -> Three String String String -> Bool)
    describe "The Semigroup instance for Four" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: Four String String String String -> Four String String String String -> Four String String String String -> Bool)
    describe "The Semigroup instance for BoolConj" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
        it "behaves as intended" $ do
            BoolConj True <> BoolConj True `shouldBe` BoolConj True
            BoolConj True <> BoolConj False `shouldBe` BoolConj False
    describe "The Semigroup instance for BoolDisj" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
        it "behaves as intended" $ do
            BoolDisj True <> BoolDisj True `shouldBe` BoolDisj True
            BoolDisj True <> BoolDisj False `shouldBe` BoolDisj True
    describe "The Semigroup instance for Or" $ do
        it "satisfies the associativity law" $ do
            property (semigroupAssoc :: Or String Bool -> Or String Bool -> Or String Bool -> Bool)
        it "behaves as intended" $ do
            Fst 1 <> Snd 2 `shouldBe` Snd 2
            Fst 1 <> Fst 2 `shouldBe` (Fst 2 :: Or Int Int) -- second type param of Or cannot be inferred so we need to specify it
            Snd 1 <> Fst 2 `shouldBe` Snd 1
            Snd 1 <> Snd 2 `shouldBe` (Snd 1 :: Or Int Int)
    describe "The Semigroup instance for Combine" $ do
        it "behaves as intended" $ do
            let f :: (Num a) => Combine a (Sum a); f = Combine $ \n -> Sum (n + 1)
            let g :: (Num a) => Combine a (Sum a); g = Combine $ \n -> Sum (n - 1)
            unCombine (f <> g) 0 `shouldBe` Sum 0
            unCombine (f <> g) 1 `shouldBe` Sum 2
            unCombine (f <> f) 1 `shouldBe` Sum 4
            unCombine (g <> f) 1 `shouldBe` Sum 2
        -- This property based test won't compile because there is no instance of either Eq or Show
        -- for the type a -> b which is a function. It's impossible to have a general definition of
        -- Eq for eg. Int -> Int because how can it know what the function does?
        -- it "satisfies the associativity law" $ do
        --     property (semigroupAssoc :: Combine Int Int -> Combine Int Int -> Combine Int Int -> Bool)
    -- The 'natural' implementation of Comp does not correspond with the behaviour of Combine. I don't know why.
    -- describe "The Semigroup instance for Comp" $ do
    --     it "behaves as intended" $ do
    --         let f :: (Num a) => Comp a; f = Comp $ \n -> n + 1
    --         let g :: (Num a) => Comp a; g = Comp $ \n -> n - 1
    --         unComp (f <> g) 0 `shouldBe` 0
    --         unComp (f <> g) 1 `shouldBe` 2
    --         unComp (f <> f) 1 `shouldBe` 4
    --         unComp (g <> f) 1 `shouldBe` 2
            
                
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

instance Arbitrary Trivial where
    arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

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
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        z' <- arbitrary
        return (Four x y z z')        
    
instance Arbitrary BoolConj where
    arbitrary = do
        x <- arbitrary
        return (BoolConj x)
    
instance Arbitrary BoolDisj where
    arbitrary = do
        x <- arbitrary
        return (BoolDisj x)        

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return $ Fst x),
                    (1, return $ Snd y) ]