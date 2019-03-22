module Foldable where

import Data.Monoid

mysum :: (Foldable t, Num a) => t a -> a
mysum y = getSum $ foldMap (\x -> Sum x) y
