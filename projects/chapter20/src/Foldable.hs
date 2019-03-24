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

minMaybe :: Ord a => a -> Maybe a -> Maybe a
minMaybe x Nothing = Just x
minMaybe x (Just y) = if x < y then Just x else Just y
