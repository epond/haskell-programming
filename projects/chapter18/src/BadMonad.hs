module BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i) (f a)

instance Applicative CountMe where
  -- pure is too opinionated
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

-- Think of it like it's Identity with an additional Integer that
-- gets incremented on each fmap or bind
instance Monad CountMe where
  return = pure
  -- Still not a valid Monad - violates right identity law
  -- CountMe _ a >>= f = f a
  CountMe n a >>= f =
    let CountMe _ b = f a
    in CountMe (n + 1) b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq


main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
