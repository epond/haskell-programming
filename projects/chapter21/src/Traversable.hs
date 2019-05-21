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
