module Chapter07MoreFunctionalPatterns where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
     where (xLast, _) = x `divMod` 10
           (_, d) = xLast `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x _ True  = x
foldBool _ x False = x

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b
    | b = x
    | otherwise = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b =
    case b of
        True  -> x
        False -> y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

roundTrip :: (Show a, Read a) => a -> a
roundTrip x = read . show $ x

-- print (roundTrip 4)

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 x = read . show $ x

-- print (roundTrip2 4 :: Int)