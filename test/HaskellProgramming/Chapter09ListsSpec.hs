module HaskellProgramming.Chapter09ListsSpec where

import Test.Hspec
import HaskellProgramming.Chapter09Lists

spec :: Spec
spec = do
    describe "Chapter09 eftBool" $ do
        it "eftBool True False gives empty list" $ do
            eftBool True False `shouldBe` []
        it "eftBool False True gives [False, True]" $ do
            eftBool False True `shouldBe` [False, True]
        it "eftBool False False gives [False]" $ do
            eftBool False False `shouldBe` [False]
        it "eftBool True True gives [True]" $ do
            eftBool True True `shouldBe` [True]

    describe "Chapter09 eftOrd" $ do
        it "eftOrd LT GT" $ do
            eftOrd LT GT `shouldBe` [LT, EQ, GT]
        it "eftOrd EQ EQ" $ do
            eftOrd EQ EQ `shouldBe` [EQ]
        it "eftOrd GT LT" $ do
            eftOrd GT LT `shouldBe` []
        it "eftOrd LT EQ" $ do
            eftOrd LT EQ `shouldBe` [LT, EQ]
        it "eftOrd EQ GT" $ do
            eftOrd EQ GT `shouldBe` [EQ, GT]

    describe "Chapter09 eftInt" $ do
        it "eftInt 3 9" $ do
            eftInt 3 9 `shouldBe` [3, 4, 5, 6, 7, 8, 9]
        it "eftInt 9 3" $ do
            eftInt 9 3 `shouldBe` []
        it "eftInt 9 9" $ do
            eftInt 9 9 `shouldBe` [9]

    describe "Chapter09 eftChar" $ do
        it "eftChar 'c' 'f'" $ do
            eftChar 'c' 'f' `shouldBe` ['c', 'd', 'e', 'f']
        it "eftChar 'f' 'c'" $ do
            eftChar 'f' 'c' `shouldBe` []
        it "eftChar 'c' 'c'" $ do
            eftChar 'c' 'c' `shouldBe` ['c']

    describe "myWords" $ do
        it "myWords all i wanna do is have some fun" $ do
            myWords "all i wanna do is have some fun" `shouldBe` ["all","i","wanna","do","is","have","some","fun"]

    describe "myLines" $ do
        it "myLines" $ do
            myLines sentences `shouldBe` [ "Tyger Tyger, burning bright"
                                         , "In the forests of the night"
                                         , "What immortal hand or eye"
                                         , "Could frame thy fearful symmetry?" ]

    describe "myZip" $ do
        it "myZip" $ do
            myZip [1, 2] [3, 4] `shouldBe` [(1, 3), (2, 4)]

    describe "Chapter Exercises" $ do
        it "has a function that filters to include only uppercase" $ do
            onlyUpper "HbEfLrLxO" `shouldBe` "HELLO"
        it "has a function that capitalises only the first letter" $ do
            capFirst "julie" `shouldBe` "Julie"
            capFirst "" `shouldBe` ""
        it "has a function that capitalises first letter and returns only it" $ do
            capAndReturnFirst "julie" `shouldBe` Just 'J'
            capAndReturnFirst "" `shouldBe` Nothing
        it "has a function that returns True if any Bool in the list is True" $ do
            myOr [] `shouldBe` False
            myOr [False] `shouldBe` False
            myOr [False, True] `shouldBe` True
            myOr [False, True, False] `shouldBe` True
        it "has a function that returns True if a function applied to any of the values in the list returns True" $ do
            myAny even [1, 3, 5] `shouldBe` False
            myAny odd [1, 3, 5] `shouldBe` True
            myAny odd [2, 3, 4] `shouldBe` True
        it "has a function that returns True if a value appears in a list" $ do
            myElem 4 [1, 2, 3] `shouldBe` False
            myElem 3 [1, 2, 3] `shouldBe` True
        it "has a function that reverses a list" $ do
            myReverse "blah" `shouldBe` "halb"
            myReverse [1..5] `shouldBe` [5, 4, 3, 2, 1]
        it "has a function to flatten lists" $ do
            squish [[1, 2], [3, 4], [5, 6]] `shouldBe` [1, 2, 3, 4, 5, 6]
        it "has a function that maps a function over a list and concatenates the result" $ do
            squishMap (\x -> [1, x, 3]) [2] `shouldBe` [1, 2, 3]
            squishMap (\x -> "WO "++[x]++" HOO ") "123" `shouldBe` "WO 1 HOO WO 2 HOO WO 3 HOO "
        it "has a function to flatten lists (reusing squishMap)" $ do
            squishAgain [[1, 2], [3, 4], [5, 6]] `shouldBe` [1, 2, 3, 4, 5, 6]
        it "has a function that finds the maximum value in a list using the supplied function" $ do
            myMaximumBy compare [1, 53, 9001, 10] `shouldBe` 9001
        it "has a function that finds the minimum value in a list using the supplied function" $ do
            myMinimumBy compare [53, 4, 9001, 10] `shouldBe` 4
        it "has a function that determines the maximum value in a list" $ do
            myMaximum [1, 53, 9001, 10] `shouldBe` 9001
        it "has a function that determines the minimum value in a list" $ do
            myMinimum [53, 4, 9001, 10] `shouldBe` 4

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen