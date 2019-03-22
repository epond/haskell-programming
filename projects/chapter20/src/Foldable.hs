module Foldable where

import Data.Monoid

mysum :: (Foldable t, Num a) => t a -> a
mysum y = getSum $ foldMap Sum y

myproduct :: (Foldable t, Num a) => t a -> a
myproduct y = getProduct $ foldMap Product y
