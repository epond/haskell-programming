module Chapter12Adversity where

import Data.List (intercalate)

-- String processing

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe msg = Just msg

replaceThe :: String -> String
replaceThe = (intercalate " ") . (map swap) . words
    where swap msg = case (notThe msg) of
            Just x -> x
            Nothing -> "a"

vowels :: [Char]
vowels = "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = (go 0) . words
    where go acc (x:y:xs) = case (notThe x) of
            Nothing | (startsWithVowel y) -> go (acc + 1) xs
            _ -> go acc (y:xs)
          go acc _ = acc

startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel (x:_) = elem x vowels

countVowels :: String -> Integer
countVowels s = toInteger . length $ filter isVowel s
    where isVowel x = elem x vowels

-- Validate the word

newtype Word' =
    Word' String
    deriving (Eq, Show)

countConsonants :: String -> Integer
countConsonants w = (toInteger . length) w - countVowels w

mkWord :: String -> Maybe Word'
mkWord w
    | countVowels w > countConsonants w = Nothing
    | otherwise                         = Just (Word' w)

-- It's only Natural

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i < 0     = Nothing
    | otherwise = Just (go i)
    where go 0 = Zero
          go x = Succ (go (x - 1))

-- Small library for Maybe

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\m rest -> case m of
    Just x -> x : rest
    Nothing -> rest) []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr (\a b -> case (a, b) of
    (Just x, Just xs) -> Just (x:xs)
    _ -> Nothing) (Just [])

-- Small library for Either

lefts' :: [Either a b] -> [a]
lefts' eithers = foldr (\a b -> case (a, b) of
    (Left x, xs) -> (x:xs)
    (_, xs) -> xs) [] eithers


rights' :: [Either a b] -> [b]
rights' eithers = foldr (\a b -> case (a, b) of
    (Right x, xs) -> (x:xs)
    (_, xs) -> xs) [] eithers

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = foldr (\a b -> case (a, b) of
    (Left x, (lefts, rights)) -> ((x:lefts), rights)
    (Right x, (lefts, rights)) -> (lefts, (x:rights))) ([], []) eithers

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\x -> Just (f x))

-- Unfolds

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:(myIterate f (f x))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case (f x) of
    Nothing -> []
    Just (a, b) -> a:(myUnfoldr f b)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (b -> Maybe (b,a,b)) -> b -> BinaryTree a
unfold f x = case (f x) of
    Nothing -> Leaf
    Just (l, y, r) -> Node (unfold f l) y (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
    where f x = if x < n then (Just (x+1, x, x+1)) else Nothing