module Applicatives where

import Control.Applicative
import Data.Monoid
import Data.List (elemIndex)

-- 17.5 Exercises: Lookups
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- 1.

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2.

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

w :: Maybe Int
w = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> w

-- 4.

xs = [1, 2, 3]
ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y' :: Maybe Integer
y' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y'

-- Exercise: Identity Instance

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity x) = Identity (f x)

-- Exercise: Constant Instance

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
    pure x = Constant mempty
    (<*>) (Constant x) (Constant y) = Constant $ mappend x y

-- 17.7 You knew this was coming

-- The checkers library can be used to check all the applicative laws for type [(String, String, Int)] like so:
--   quickBatch $ applicative ([("b", "w", 1)])
-- The value passed to applicative is not used; only its type is of interest to determine the Arbitrary instance.

-- Instead, bottom can be used with explicit type to achieve the same effect:
--   quickBatch $ applicative (undefined :: [(String, String, Int)])

-- 17.8 List Applicative Exercise

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    -- fmap :: (a -> b) -> f a -> f b
    fmap _ Nil = Nil
    fmap f (Cons x rest) = Cons (f x) $ fmap f rest

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Monoid (List a) where
    mempty = Nil
    x `mappend` y = x `append` y

instance Applicative List where
    -- pure :: a -> f a
    pure x = Cons x (Nil)
    -- (<*>) :: f (a -> b) -> f a -> f b
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)

runListApp :: List Int
runListApp = f <*> v
    where f = Cons (+1) (Cons (*2) Nil)
          v = Cons 1 (Cons 2 Nil)
          
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
-- This implementation of pure fails identity, composition and functor applicative laws
--     pure x = ZipList' $ Cons x Nil
    pure x = ZipList' $ repeat' x
    (ZipList' Nil) <*> _ = ZipList' Nil
    _ <*> (ZipList' Nil) = ZipList' Nil
-- This was my first attempt but id can be used instead of (\f -> \x -> f x)
--     (ZipList' fs) <*> (ZipList' xs) = ZipList' $ zipWith' (\f -> \x -> f x) fs xs
    (ZipList' fs) <*> (ZipList' xs) = ZipList' $ zipWith' id fs xs

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f = go
    where
        go Nil _ = Nil
        go _ Nil = Nil
        go (Cons x xs) (Cons y ys) = Cons (f x y) (go xs ys)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

repeat' :: a -> List a
repeat' x = Cons x $ repeat' x