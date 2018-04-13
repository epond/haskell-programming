module HaskellProgramming.Chapter15Monoids where

import Data.Monoid

-- Optional Monoid exercise

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = mempty

    mappend (Only x) (Only y) = Only (mappend x y)
    mappend (Only x) Nada = Only (x)
    mappend Nada (Only y) = Only (y)
    mappend Nada Nada = Nada

-- Madness

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his convertible " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat [e, "! he said ", adv, " as he jumped into his convertible ", noun, " and drove off with his ", adj, " wife."]

-- Better living through QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Testing QuickCheck's patience

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

-- Exercise: Maybe Another Monoid

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' Nada

    mappend (First' (Only x)) (First' (Only _)) = First' (Only x)
    mappend (First' (Only x)) (First' Nada) = First' (Only x)
    mappend (First' Nada) (First' (Only x)) = First' (Only x)
    mappend (First' Nada) (First' Nada) = First' Nada