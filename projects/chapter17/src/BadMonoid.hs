module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Property testing the Monoid laws with QuickCheck and the checkers library.
-- The Monoid laws are bundled into a TestBatch called monoid.

data Bull =
    Fools
  | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary =
        frequency [ (1, return Fools)
                  , (1, return Twoo) ]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

-- We need to define EqProp for our custom datatype.
-- checkers exports a function called eq which reuses the existing Eq instance for the datatype.
instance EqProp Bull where (=-=) = eq

main :: IO ()
-- We're passing a value of our datatype to monoid so it knows which Arbitrary instance to use.
-- It doesn't use this value for anything.
main = quickBatch (monoid Twoo)