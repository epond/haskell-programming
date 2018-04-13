module HaskellProgramming.Chapter11Datatypes where

import Data.Char (toUpper, isUpper, toLower)
import Data.List (intercalate, elemIndex, sortOn)

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a  = Node (insert' b left) a right
    | b > a  = Node left a (insert' b right)
insert' _ t = t

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left a right) = f a l
    where l = foldTree f r left
          r = foldTree f z right

-- As-patterns

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) l = elem x l && isSubsequenceOf xs l

capitaliseWords :: String -> [(String, String)]
capitaliseWords s = map tupelify (words s)
    where tupelify w@(x:xs) = (w, (toUpper x) : xs)
          tupelify _ = ("", "")

capitaliseWord :: String -> String
capitaliseWord [] = []
capitaliseWord (x:xs) = (toUpper x) : xs

-- Language exercises

isStopWord :: String -> Bool
isStopWord [] = False
isStopWord w = (head . reverse) w == '.'

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

dropWhileExclusive :: (a -> Bool) -> [a] -> [a]
dropWhileExclusive _ [] = []
dropWhileExclusive _ (_:[]) = []
dropWhileExclusive p (_:x':xs) = if p x' then dropWhileExclusive p xs else xs

sentences :: String -> [String]
sentences [] = []
sentences p = (capitaliseWord . glue) (takeWhileInclusive (not . isStopWord) (words p)) : sentences (glue (dropWhileExclusive (not . isStopWord) (words p)))
    where glue = intercalate " "

capitaliseParagraph :: String -> String
capitaliseParagraph p = intercalate " " $ sentences p

-- Phone exercises

data PhoneButton = TextButton String

data DaPhone = DaPhone [PhoneButton]

standardLayout :: DaPhone
standardLayout = DaPhone [
    TextButton "1",
    TextButton "abc2",
    TextButton "def3",
    TextButton "ghi4",
    TextButton "jkl5",
    TextButton "mno6",
    TextButton "pqrs7",
    TextButton "tuv8",
    TextButton "wxy9",
    TextButton " 0",
    TextButton ".,#"
    ]

type Digit = Char
type Presses = Int

reverseTapsAlwaysLower :: DaPhone -> Char -> [(Digit, Presses)]
reverseTapsAlwaysLower (DaPhone []) _ = []
reverseTapsAlwaysLower (DaPhone ((TextButton glyphs):xs)) char = case elemIndex char glyphs of
    Just index -> [((head . reverse) glyphs, index + 1)]
    Nothing -> reverseTapsAlwaysLower (DaPhone xs) char

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps layout char = if isUpper char then ('*', 1) : rest else rest
    where rest = reverseTapsAlwaysLower layout (toLower char)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead layout message = concat $ map (reverseTaps layout) message

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps list = sum $ map snd list

distinctLetters :: String -> [Char]
distinctLetters = go [] where
    go acc [] = acc
    go acc (x:xs) = if elem x acc then go acc xs else go (x:acc) xs

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

mostPopularLetter :: String -> Char
mostPopularLetter message = fst . head . reverse $
    sortOn snd (map letterAndCount (distinctLetters message))
    where letterAndCount letter = (letter, count letter message)

mostPopularLetterCost :: DaPhone -> String -> Presses
mostPopularLetterCost layout message = letterFreq * letterCost
    where letter = mostPopularLetter message
          letterFreq = length (filter (== letter) message)
          letterCost = fingerTaps (reverseTaps layout letter)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord conversation = fst . head . reverse $
    sortOn snd (map wordAndCount wordList)
    where wordAndCount word = (word, count word wordList)
          wordList = words $ intercalate " " conversation

-- Hutton's Razor

data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add x y) = (printExpr x) ++ " + " ++ (printExpr y)