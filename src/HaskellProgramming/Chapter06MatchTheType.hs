import Data.List

i :: Num a => a
i = 1
-- can signature be replaced with i :: a ?
-- No because we need an instance of Num

f :: Float
f = 1.0 -- The literal `1.0` requires a `Fractional` to work
-- can signature be replaced with f :: Num a => a ?
--  No because Num does not imply Fractional
-- can signature be replaced with f :: Fractional a => a ?
--  Yes because 1.0 has default type of Fractional a => a
-- can signature be replaced with f :: RealFrac a => a ?
--  Yes because RealFrac implies Fractional

freud :: a -> a
freud x = x
-- can signature be replaced with freud :: Ord a => a -> a ?
--  Yes because x is fully polymorphic
-- can signature be replaced with freud :: Int -> Int ?
--  Yes because input and return types match

myX = 1 :: Int
sigmund :: Int -> Int
sigmund x = myX
-- can signature be replaced with sigmund :: a -> a ?
--  No because that would allow non-Ints as input when it must always return an Int
-- can signature be replaced with sigmund :: Num a => a -> a ?
--  No for the same reason as before

jung :: Ord a => [a] -> a
jung xs = head (sort xs)
-- can signature be replaced with jung :: [Int] -> Int ?
--  Yes

young :: [Char] -> Char
young xs = head (sort xs)
-- can signature be replaced with young :: Ord a => [a] -> a ?
--  Yes as was proven in the previous example

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)
-- can signature be replaced with signifier :: Ord a => [a] -> a ?
--  No because mySort restricts list values to Char