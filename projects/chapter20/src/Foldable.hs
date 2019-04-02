module Foldable where

import Data.Monoid
import Data.Semigroup

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
