{-# LANGUAGE InstanceSigs #-}

module Reader where

import Control.Applicative
import Data.Char

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop


-- the argument will get passed to both boop and doop in parallel,
-- and the results will be added together.
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- (+) <$> (*2) is identical to (+) . (*2)

-- The Functor of functions is function composition.
-- The Applicative and Monad chain the argument forward.
-- This is the idea of Reader.


-- Short Exercise: Warming Up

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

-- or using monadic do:
--tupled = do
--  x <- cap
--  y <- rev
--  return (x,y)

-- or usding monadic >>=:
--tupled = cap >>= (\x -> rev >>= (\y -> return (x,y)))


newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

-- Demonstrating the function Applicative

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName
, dogName :: DogName
, address :: Address
} deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName
, dogsAddress :: Address
} deriving (Eq, Show)

bigbird :: Person
bigbird = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)
-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address
-- with Reader, alternate
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address


-- Exercise: Reading Comprehension
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap :: (a -> b)
       -> Reader r a
       -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ (\_ -> a)

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> (rab r) (ra r)
