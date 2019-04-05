module Foldable where

import Data.Monoid
import Data.Semigroup

-- 20.5 Exercises

mysum :: (Foldable t, Num a) => t a -> a
mysum = getSum . foldMap Sum

myproduct :: (Foldable t, Num a) => t a -> a
myproduct = getProduct . foldMap Product

myelem :: (Foldable t, Eq a) => a -> t a -> Bool
myelem x = getAny . foldMap (\y -> Any (x == y))

myminimum :: (Foldable t, Ord a) => t a -> Maybe a
myminimum = foldr minMaybe Nothing
  where minMaybe x Nothing = Just x
        minMaybe x (Just y) = if x < y then Just x else Just y

mymaximum :: (Foldable t, Ord a) => t a -> Maybe a
mymaximum = foldr maxMaybe Nothing
  where maxMaybe x Nothing = Just x
        maxMaybe x (Just y) = if x > y then Just x else Just y

mynull :: (Foldable t) => t a -> Bool
mynull = getAll . foldMap (\_ -> All False)

mylength :: Foldable t => t a -> Int
mylength = getSum . foldMap (Sum . (const 1))

mytoList :: Foldable t => t a -> [a]
mytoList = foldr mklist []
  where mklist x xs = x : xs

myfold :: (Foldable t, Monoid m) => t m -> m
myfold = foldMap id

myfoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myfoldMap f = foldr (\x y -> (f x) `mappend` y) mempty

-- 20.6 Chapter Exercises

-- 1.
data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x

-- 2.
data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ x) = f x

-- 3.
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x

-- 4.
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ _ x) = f x

-- 5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ _ _ x) = f x

-- Write a filter function for Foldable types using foldMap
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap foldF
  where foldF x = if (f x) then (pure x) else mempty
