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
      mysum [3,7,5] `shouldBe` 15
      mysum (9, 4) `shouldBe` 4
      mysum Nothing `shouldBe` 0
    it "myproduct :: (Foldable t, Num a) => t a -> a" $ do
      myproduct (Just 3) `shouldBe` 3
      myproduct [3,7,5] `shouldBe` 105
      myproduct (9, 4) `shouldBe` 4
      myproduct Nothing `shouldBe` 1
    it "myelem :: (Foldable t, Eq a) => a -> t a -> Bool" $ do
      myelem 2 [1,2,3] `shouldBe` True
      myelem 2 (Just 3) `shouldBe` False
      myelem 3 (Just 3) `shouldBe` True
      myelem 3 Nothing `shouldBe` False
      fmap (myelem 3) [Right 1, Right 2, Right 3] `shouldBe` [False, False, True]
    it "myminimum :: (Foldable t, Ord a) => t a -> Maybe a" $ do
      myminimum "minute" `shouldBe` Just 'e'
      myminimum (Nothing :: Maybe Int) `shouldBe` Nothing
      myminimum (Left 'a' :: Either Char Char) `shouldBe` Nothing
    it "mymaximum :: (Foldable t, Ord a) => t a -> Maybe a" $ do
      mymaximum "minute" `shouldBe` Just 'u'
      mymaximum (Nothing :: Maybe Int) `shouldBe` Nothing
      mymaximum (Left 'a' :: Either Char Char) `shouldBe` Nothing
    it "mynull :: (Foldable t) => t a -> Bool" $ do
      mynull (Left 3) `shouldBe` True
      mynull [] `shouldBe` True
      mynull Nothing `shouldBe` True
      mynull (1, 3) `shouldBe` False
      mynull [1] `shouldBe` False
    it "mylength :: (Foldable t, Num a) => t a -> a" $ do
      mylength (Just 3) `shouldBe` 1
      mylength [3,7,5] `shouldBe` 3
      mylength (9, 4) `shouldBe` 1
      mylength Nothing `shouldBe` 0
    it "mytoList :: Foldable t => t a -> [a]" $ do
      mytoList (Nothing :: Maybe Bool) `shouldBe` []
      mytoList (Just 3) `shouldBe` [3]
      mytoList (Right 'c') `shouldBe` ['c']
      mytoList [2,5,7] `shouldBe` [2,5,7]
    it "myfold :: (Foldable t, Monoid m) => t m -> m" $ do
      myfold [] `shouldBe` ()
      myfold [Sum 2, Sum 4] `shouldBe` Sum 6
      myfold [Product 2, Product 4] `shouldBe` Product 8
    it "myfoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m" $ do
      myfoldMap Sum [1, 3, 5] `shouldBe` Sum 9
      myfoldMap Product [1, 3, 5] `shouldBe` Product 15
  describe "20.6: Chapter Exercises" $ do
    it "Foldable instance for Constant" $ do
      foldMap Sum (Constant 3) `shouldBe` Sum 3
      fold (Constant (Sum 4)) `shouldBe` Sum 4
    it "Foldable instance for Two" $ do
      foldMap Sum (Two 3 5) `shouldBe` Sum 5
      fold (Two 3 (Sum 5)) `shouldBe` Sum 5
    it "Foldable instance for Three" $ do
      foldMap Sum (Three 'q' False 9) `shouldBe` Sum 9
      fold (Three 'q' False (Sum 9)) `shouldBe` Sum 9
    it "Foldable instance for Three'" $ do
      foldMap Sum (Three' 'q' 2 9) `shouldBe` Sum 9
      fold (Three' 'q' 2 (Sum 9)) `shouldBe` Sum 9
    it "Foldable instance for Four'" $ do
      fold (Four' False 1 3 9) `shouldBe` Sum 9
    it "Filter function for Foldable types using foldMap" $ do
      filterF even [1,2,3,4,5] `shouldBe` [2,4]
