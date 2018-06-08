{-# LANGUAGE FlexibleInstances #-}

module Chapter16Functors where

-- Commonly used functors
-- Wait, how does that even typecheck? Work through the types to see how the
-- compiler arrived at the type for (fmap . fmap)
-- (.) fmap fmap :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)


-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- fmap :: Functor f => (m -> n) -> (f m -> f n)
-- fmap :: Functor g => (x -> y) -> (g x -> g y)

-- b = (m -> n)
-- c = (f m -> f n)
-- (.) :: ((m -> n) -> (f m -> f n)) -> (a -> (m -> n)) -> a -> (f m -> f n)
-- (.) fmap :: (a -> (m -> n)) -> a -> (f m -> f n)

-- m = g x
-- n = g y
-- (.) :: (g x -> g y) -> (f (g x) -> f (g y)) -> (a -> (g x -> g y)) -> a -> (f (g x) -> f (g y))

-- a = (x -> y)
-- (.) :: (g x -> g y) -> (f (g x) -> f (g y)) -> ((x -> y) -> (g x -> g y)) -> (x -> y) -> (f (g x) -> f (g y))
-- (.) fmap :: ((x -> y) -> (g x -> g y)) -> (x -> y) -> (f (g x) -> f (g y))
-- (.) fmap fmap :: (x -> y) -> (f (g x) -> f (g y))
-- (.) fmap fmap :: (x -> y) -> f (g x) -> f (g y)


-- Exercises: Heavy Lifting

-- a = (+1) $ read "[1]" :: [Int]
-- Expected a == [2]
a :: [Int]
a = fmap (+1) $ read "[1]"

-- b = (++ "lol") (Just ["Hi,", "Hello"])
-- Expected b == Just ["Hi,lol","Hellolol"]
b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- c = (*2) (\x -> x - 2)
-- Expected c 1 == -2
c :: Num a => a -> a
c = fmap (*2) (\x -> x - 2)

-- d = ((return '1' ++) . show) (\x -> [x, 1..3])
-- Expected d 0 == "1[0,1,2,3]"
d :: (Enum t, Num t, Show t) => t -> [Char]
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) show ioi
--     in (*3) changed
-- Expected e == 3693


-- 16.9 QuickChecking Functor instances

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- ghci> let f = (\x -> functorIdentity x) :: [Int] -> Bool
-- ghci> import Test.QuickCheck
-- ghci> quickCheck f

-- ghci> let c = functorCompose (+1) (*2)
-- ghci> let li x = c (x :: [Int])
-- ghci> quickCheck li


-- 16.10 Exercises: Instances of Func

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four w x y z) = Four w x y (f z)

data Four' a b c = Four' a b c c deriving (Eq, Show)

instance Functor (Four' a b) where
    fmap f (Four' w x y z) = Four' w x (f y) (f z)

-- Cannot implement Functor for Trivial because its kind is * and Functor needs * -> *
data Trivial = Trivial


-- 16.11 Ignoring Possibilities. Exercise: Possibly

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap f LolNope = LolNope
    fmap f (Yeppers x) = Yeppers (f x)

-- Short Exercise

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First x) = First x
    fmap f (Second x) = Second $ f x


-- 16.17 Chapter exercises

-- Can a valid Functor be written for each type?

-- 1.
-- data Bool = False | True
-- No because type has kind * but we need it to have kind * -> *

-- 2.
-- data BoolAndSomethingElse a = False' a | True' a
-- Yes because it has kind * -> *

-- 3.
-- data BoolAndMaybeSomethingElse a = Falsish | Truish a
-- Yes, this is Maybe

-- 4.
-- newtype Mu f = InF { outF :: f (Mu f) }
-- No because type has kind (* -> *) -> *

-- 5.
-- data D = D (Array Word Word) Int Int
-- Yes because it has kind * -> *


-- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.

-- 1. data Summ a b = Fst a | Snd b

data Summ b a = Fst a | Snd b

instance Functor (Summ e) where
    fmap f (Fst a) = Fst (f a)
    fmap f (Snd b) = Snd b

-- 2. data Company a b c = DeepBlue a c | Something b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

-- 3. data More a b = L a b a | R b a b deriving (Eq, Show)

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'


-- Write Functor instances for the following datatypes,

-- 1.

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk x) = Desk x
    fmap f (Bloor x) = Bloor (f x)

-- 2.

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap _ (K x) = K x

-- 3.

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
    fmap f (Flip (K' x)) = Flip (K' (f x))

-- 4.

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst x) = GoatyConst (f x)

-- 5.

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut f) = LiftItOut (fmap g f)

-- 6.

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap h (DaWrappa f g) = DaWrappa (fmap h f) (fmap h g)

-- 7.

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap h (IgnoringSomething f g) = IgnoringSomething f (fmap h g)

-- 8.

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious o a t) = Notorious o a (fmap f t)