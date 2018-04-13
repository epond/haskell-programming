module HaskellProgramming.Chapter11DatatypesSpec where

import Test.Hspec
import HaskellProgramming.Chapter11Datatypes

spec :: Spec
spec = do
    describe "A binary tree" $ do
        it "can insert to create a new tree" $ do
            insert' 0 Leaf `shouldBe` Node Leaf 0 Leaf
        it "can insert to a tree of one level" $ do
            insert' 5 (insert' 0 Leaf) `shouldBe` Node Leaf 0 (Node Leaf 5 Leaf)
        it "can insert to a tree of two levels" $ do
            insert' 3 (insert' 5 (insert' 0 Leaf)) `shouldBe` Node Leaf 0 (Node (Node Leaf 3 Leaf) 5 Leaf)
        it "can map a function over each node" $ do
            let testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
            let mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
            mapTree (+1) testTree' `shouldBe` mapExpected
        it "can convert to a preordered list" $ do
            preorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) `shouldBe` [2, 1, 3]
        it "can convert to an inordered list" $ do
            inorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) `shouldBe` [1, 2, 3]
        it "can convert to a postordered list" $ do
            postorder (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) `shouldBe` [1, 3, 2]
        it "has a function to perform a catamorphism" $ do
            foldTree (+) 0 (Node (Node Leaf 4 Leaf) 7 (Node Leaf 3 Leaf)) `shouldBe` 14
            foldTree (*) 1 (Node (Node Leaf 4 Leaf) 7 (Node Leaf 3 Leaf)) `shouldBe` 84
    describe "As-patterns" $ do
        it "can be used to implement a subsequence function" $ do
            isSubsequenceOf "blah" "blahwoot" `shouldBe` True
            isSubsequenceOf "blah" "wootblah" `shouldBe` True
            isSubsequenceOf "blah" "wboloath" `shouldBe` True
            isSubsequenceOf "blah" "wootbla" `shouldBe` False
            isSubsequenceOf "" "blah" `shouldBe` True
            isSubsequenceOf "b" "" `shouldBe` False
        it "can be used to implement a function that capitalises words in a tuple" $ do
            capitaliseWords "hello world" `shouldBe` [("hello", "Hello"), ("world", "World")]
    describe "Language exercises" $ do
        it "can capitalise a word" $ do
            capitaliseWord "Hello" `shouldBe` "Hello"
            capitaliseWord "hello" `shouldBe` "Hello"
        it "can capitalise two sentences in a paragraph" $ do
            capitaliseParagraph "foo blah. woot ha." `shouldBe` "Foo blah. Woot ha."
        it "can capitalise three sentences in a paragraph" $ do
            capitaliseParagraph "foo blah. woot ha. nice." `shouldBe` "Foo blah. Woot ha. Nice."
    describe "Phone exercises" $ do
        it "can convert a character into button presses" $ do
            reverseTaps standardLayout 'y' `shouldBe` [('9', 3)]
            reverseTaps standardLayout '3' `shouldBe` [('3', 4)]
            reverseTaps standardLayout ' ' `shouldBe` [('0', 1)]
            reverseTaps standardLayout 'Q' `shouldBe` [('*', 1), ('7', 2)]
        it "can convert a string into button presses" $ do
            cellPhonesDead standardLayout "Ya" `shouldBe` [('*', 1), ('9', 3), ('2', 1)]
            cellPhonesDead standardLayout "U 1st" `shouldBe` [('*', 1), ('8', 2), ('0', 1), ('1', 1), ('7', 4), ('8', 1)]
        it "can calculate how many button presses are needed for a given message" $ do
            fingerTaps [('*', 1), ('9', 3), ('2', 1)] `shouldBe` 5
            fingerTaps [('*', 1), ('8', 2), ('0', 1), ('1', 1), ('7', 4), ('8', 1)] `shouldBe` 10
        it "can calculate the most popular letter from a String" $ do
            mostPopularLetter "a" `shouldBe` 'a'
            mostPopularLetter "aab" `shouldBe` 'a'
            mostPopularLetter "abb" `shouldBe` 'b'
            mostPopularLetter "a bb cccc ddd" `shouldBe` 'c'
            mostPopularLetter "a bb cCCc dddd" `shouldBe` 'd'
            mostPopularLetter "a bb CCCC ddd" `shouldBe` 'C'
        it "can calculate the cost of the most popular letter" $ do
            mostPopularLetterCost standardLayout "a" `shouldBe` 1
            mostPopularLetterCost standardLayout "a bb cCCc yyyy" `shouldBe` 12
            mostPopularLetterCost standardLayout "a bb CCCC yyy" `shouldBe` 16
        it "can calculate the most popular letter from a List of String" $ do
            coolestLtr ["a","bb","cccc","ddd"] `shouldBe` 'c'
        it "can calculate the most popular word" $ do
            coolestWord ["baa", "baa", "black", "sheep"] `shouldBe` "baa"
    describe "Hutton's Razor" $ do
        it "can reduce an expression to its final sum" $ do
            eval (Add (Lit 1) (Lit 9001)) `shouldBe` 9002
        it "can print an expression as a String" $ do
            printExpr (Add (Lit 1) (Lit 9001)) `shouldBe` "1 + 9001"
            let a1 = Add (Lit 9001) (Lit 1)
            let a2 = Add a1 (Lit 20001)
            let a3 = Add (Lit 1) a2
            printExpr a3 `shouldBe` "1 + 9001 + 1 + 20001"