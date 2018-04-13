module HaskellProgramming.Chapter10FoldingLists where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
                (fromGregorian 1911 5 1)
                (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate u) : rest) = u : (filterDbDate rest)
filterDbDate (_ : rest) = filterDbDate rest

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber n) : rest) = n : (filterDbNumber rest)
filterDbNumber (_ : rest) = filterDbNumber rest

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = foldr max (UTCTime (fromGregorian 1 1 1) (secondsToDiffTime 0)) (filterDbDate db)

sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (+) 0 (filterDbNumber db)

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (length db)

-- Rewriting functions using folds

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> y || f x) False

-- using folding
--myElem :: Eq a => a -> [a] -> Bool
--myElem x = foldr (\a b -> x == a || b) False

-- using any
myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny $ (==) x

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x ys -> (f x) : ys) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x ys -> if (f x)
                             then x : ys
                             else ys) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> if f a b == GT then a else b) (last xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (last xs) xs