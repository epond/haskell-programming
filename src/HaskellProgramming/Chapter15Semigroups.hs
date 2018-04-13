module HaskellProgramming.Chapter15Semigroups where

import Data.Semigroup

-- Semigroup exercises

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
    Identity x <> _ = Identity x

data Two a b = Two a b deriving (Eq, Show)

instance Semigroup (Two a b) where
    (Two x y) <> _ = Two x y

data Three a b c = Three a b c deriving (Eq, Show)

instance Semigroup (Three a b c) where
    (Three x y z) <> _ = Three x y z

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Semigroup (Four a b c d) where
    (Four x y z z') <> _ = Four x y z z'

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)    

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    Fst _ <> Fst x = Fst x
    Fst _ <> Snd x = Snd x
    Snd x <> Fst _ = Snd x
    Snd x <> Snd _ = Snd x

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (\x -> f x <> g x)

newtype Comp a = Comp { unComp :: (a -> a) }

-- It's unclear if this is the correct implementation
instance Semigroup (Comp a) where
    Comp f <> Comp g = Comp (f . g)