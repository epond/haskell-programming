module Traversable where

-- 21.12 Chapter Exercises

-- Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

-- Constant
newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldr f z (Constant _) = z

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure $ Constant x

-- Maybe
data Optional a = Nada | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap f (Yep x) = Yep (f x)
  fmap _ Nada = Nada

instance Foldable Optional where
  foldMap f (Yep x) = f x
  foldMap _ Nada = mempty

instance Traversable Optional where
  traverse f (Yep x) = Yep <$> f x
  traverse _ Nada = pure Nada

-- List
data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = mappend (f x) (foldMap f xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

-- (<$>) ::                         Functor f =>   (a -> b) -> f a -> f b
-- (<*>) ::                     Applicative f => f (a -> b) -> f a -> f b
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- Three
data Three a b c = Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = (Three x y) <$> (f z)

-- Pair
data Pair a b = Pair a b
  deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Foldable (Pair a) where
  foldMap f (Pair x y) = f y

instance Traversable (Pair a) where
  traverse f (Pair x y) = (Pair x) <$> (f y)

-- Big
data Big a b = Big a b b
  deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big x y z) = (f y) `mappend` (f z)

instance Traversable (Big a) where
  traverse f (Big x y z) = (Big x) <$> (f y) <*> (f z)
