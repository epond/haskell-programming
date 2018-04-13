module HaskellProgramming.Chapter09Lists where

import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
    | x == y    = [x]
    | x < y     = [x, y]
    | otherwise = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
    | x == y    = [x]
    | x < y     = x : eftOrd (succ x) y
    | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt x y
    | x == y    = [x]
    | x < y     = x : eftInt (succ x) y
    | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar x y
    | x == y    = [x]
    | x < y     = x : eftChar (succ x) y
    | otherwise = []

myWords :: String -> [String]
myWords = myWay ' '

myLines :: String -> [String]
myLines = myWay '\n'

myWay :: Char -> String -> [String]
myWay _ "" = []
myWay c s
    | c == (head s) = myWay c (tail s)
    | otherwise = takeWhile (/= c) s : myWay c (dropWhile (/= c) s)

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

onlyUpper :: [Char] -> [Char]
onlyUpper = filter isUpper

capFirst :: [Char] -> [Char]
capFirst [] = []
capFirst (x : xs) = (toUpper x) : xs

capAndReturnFirst :: [Char] -> Maybe Char
capAndReturnFirst [] = Nothing
capAndReturnFirst xs = Just (toUpper . head $ xs)

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myElem :: Eq a => a -> [a] -> Bool
--myElem _ [] = False
--myElem a (x : xs) = a == x || myElem a xs
myElem x = myAny (\y -> x == y)

myReverse :: [a] -> [a]
myReverse _l = doReverse _l []
    where doReverse [] acc = acc
          doReverse (x : xs) acc = doReverse xs (x : acc)

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x : []) = x
myMaximumBy f (x : y : xs) = case f x y of
    GT -> myMaximumBy f (x : xs)
    _  -> myMaximumBy f (y : xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x : []) = x
myMinimumBy f (x : y : xs) = case f x y of
    LT -> myMinimumBy f (x : xs)
    _  -> myMinimumBy f (y : xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare