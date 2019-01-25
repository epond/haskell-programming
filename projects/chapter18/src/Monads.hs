module Monads where

-- 18.4 Examples of Monad use

data Cow = Cow {
  name :: String
, age :: Int
, weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

-- Do syntax isn't just for IO.
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

-- Stack up the nested lambdas.
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  \nammy ->
    noNegative age' >>=
    \agey ->
      noNegative weight' >>=
      \weighty -> weightCheck (Cow nammy agey weighty)


-- Short Exercise: Either Monad
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure = Second
  
  Second f <*> Second x = Second (f x)
  Second _ <*> First x  = First x
  First x  <*> _        = First x

instance Monad (Sum a) where
  return = pure

  First x >>= _  = First x
  Second x >>= f = f x


-- 18.7 Chapter Exercises
-- Write Monad instances
-- 1.
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

-- 2.
data PhhhbbtttEither b a = Left a | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither a) where
  fmap _ (Monads.Right x) = Monads.Right x
  fmap f (Monads.Left x) = Monads.Left (f x)

instance Applicative (PhhhbbtttEither a) where
  pure = Monads.Left
  _ <*> Monads.Right e = Monads.Right e
  Monads.Right e <*> _ = Monads.Right e
  Monads.Left f <*> Monads.Left x = Monads.Left (f x)

instance Monad (PhhhbbtttEither a) where
  return = pure
  Monads.Right e >>= _ = Monads.Right e
  Monads.Left x >>= f = f x
