module Monads where

import Data.Monoid ((<>))
import Control.Monad (join)
import Control.Applicative (liftA2)

-- 18.4 Examples of Monad use

data Cow = Cow {
  name :: String
, age :: Int
, weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

-- Do syntax isn't just for IO.
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

-- Stack up the nested lambdas.
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  \nammy ->
    noNegative age' >>=
    \agey ->
      noNegative weight' >>=
      \weighty -> weightCheck (Cow nammy agey weighty)


-- Short Exercise: Either Monad
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure = Second
  
  Second f <*> Second x = Second (f x)
  Second _ <*> First x  = First x
  First x  <*> _        = First x

instance Monad (Sum a) where
  return = pure

  First x >>= _  = First x
  Second x >>= f = f x


-- 18.7 Chapter Exercises
-- Write Monad instances
-- 1.
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

-- 2.
data PhhhbbtttEither b a = Left a | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither a) where
  fmap _ (Monads.Right x) = Monads.Right x
  fmap f (Monads.Left x) = Monads.Left (f x)

instance Applicative (PhhhbbtttEither a) where
  pure = Monads.Left
  _ <*> Monads.Right e = Monads.Right e
  Monads.Right e <*> _ = Monads.Right e
  Monads.Left f <*> Monads.Left x = Monads.Left (f x)

instance Monad (PhhhbbtttEither a) where
  return = pure
  Monads.Right e >>= _ = Monads.Right e
  Monads.Left x >>= f = f x

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

-- 4.
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

concat' :: List (List a) -> List a
concat' = fold append Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x rest) = Cons (f x) $ fmap f rest

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

instance Monad List where
  return x = Cons x Nil

  -- (>>=) :: List a -> (a -> List b) -> List b
  (>>=) l f = concat' $ fmap f l


-- Write using the methods provided by Monad and Functor.
--class Applicative m => Monad (m :: * -> *) where
--  (>>=) :: m a -> (a -> m b) -> m b
--  (>>) :: m a -> m b -> m b
--  return :: a -> m a
--  fail :: String -> m a
--class Functor (f :: * -> *) where
--  fmap :: (a -> b) -> f a -> f b
--  (<$) :: a -> f b -> f a

-- 1.
j :: Monad m => m (m a) -> m a
j x = x >>= id

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.
-- liftA2 is explained in Chapter 17.5 Applicative in use
-- (,) <$> [1,2] <*> [3,4] = [(1,3),(1,4),(2,3),(2,4)]
-- liftA2 (,) [1,2] [3,4]  = [(1,3),(1,4),(2,3),(2,4)]
-- l2 (,) [1,2] [3,4]      = [(1,3),(1,4),(2,3),(2,4)]
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x = (<*>) (fmap f x)

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = undefined --(f x) >>= 
