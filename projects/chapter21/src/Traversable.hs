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
