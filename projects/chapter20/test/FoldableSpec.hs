module FoldableSpec where

import Foldable
import Data.Foldable
import Data.Monoid
import Test.Hspec

spec :: Spec
spec = do
  describe "20.3 Revenge of the Monoids" $ do
    it "foldr can fold integers using the addition binary function" $ do
      foldr (+) 0 [1..5] `shouldBe` 15
    it "fold can fold integers using the Sum Monoid instance" $ do
      getSum (fold (map Sum [1..5])) `shouldBe` 15
    it "fold can fold integers using the Product Monoid instance" $ do
      getProduct (fold (map Product [1..5])) `shouldBe` 120
    it "fold can find the standard Monoid for lists without having to specify it" $ do
      fold ["hello", " world"] `shouldBe` "hello world"
    it "foldMap can fold using a function that maps to a Monoid" $ do
      getSum (foldMap Sum [1..5]) `shouldBe` 15
      getAll (foldMap All [True, False, True]) `shouldBe` False
      getFirst (foldMap First [Just 1, Nothing, Just 5]) `shouldBe` Just 1
    it "foldMap can have a different mapping function to the Monoid its using" $ do
      getProduct (foldMap (*5) (map Product [1..3])) `shouldBe` 750
  describe "20.5 Exercises: Library functions - implement in terms of foldMap and foldr" $ do
    it "mysum :: (Foldable t, Num a) => t a -> a" $ do
      mysum (Just 3) `shouldBe` 3
