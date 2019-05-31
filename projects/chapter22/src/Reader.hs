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
