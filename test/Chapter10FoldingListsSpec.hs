module Chapter10FoldingListsSpec where

import Test.Hspec
import Chapter10FoldingLists
import Data.Time

spec :: Spec
spec = do
    describe "Database Processing" $ do
        it "has a function that filters for DbDate and returns a list of UTCTime values inside them" $ do
            filterDbDate theDatabase `shouldBe` [UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123), UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)]
        it "has a function that filters for DbNumber and returns a list of the Integer values inside them" $ do
            filterDbNumber theDatabase `shouldBe` [9001]
        it "has a function that returns the most recent time" $ do
            mostRecent theDatabase `shouldBe` UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)
        it "has a function that sums up all the DbNumbers" $ do
            let db1 = []
            sumDb db1 `shouldBe` 0
            let db2 = [DbNumber 24, DbString "5", DbNumber 76]
            sumDb db2 `shouldBe` 100
        it "has a function that finds the average of the DbNumbers" $ do
            let db = [DbNumber 10, DbNumber 20]
            avgDb db `shouldBe` 15.0
    describe "Rewriting functions using folds" $ do
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
        it "has a mapping function" $ do
            myMap (+1) [1,2,3] `shouldBe` [2,3,4]
            myMap length ["Hi", "Dave"] `shouldBe` [2,4]
        it "has a filter function" $ do
            myFilter even [1..7] `shouldBe` [2,4,6]
        it "has a function to flatten lists" $ do
            squish [[1, 2], [3, 4], [5, 6]] `shouldBe` [1, 2, 3, 4, 5, 6]
        it "has a function that maps a function over a list and concatenates the result" $ do
            squishMap (\x -> [1, x, 3]) [2] `shouldBe` [1, 2, 3]
            squishMap (\x -> "WO "++[x]++" HOO ") "123" `shouldBe` "WO 1 HOO WO 2 HOO WO 3 HOO "
        it "has a function to flatten lists, reusing squishMap" $ do
            squishAgain [[1, 2], [3, 4], [5, 6]] `shouldBe` [1, 2, 3, 4, 5, 6]
        it "has a function that finds the maximum value in a list using the supplied function" $ do
            myMaximumBy (\_ _ -> GT) [1..10] `shouldBe` 1
            myMaximumBy (\_ _ -> LT) [1..10] `shouldBe` 10
            myMaximumBy compare [1, 53, 9001, 10] `shouldBe` 9001
        it "has a function that finds the minimum value in a list using the supplied function" $ do
            myMinimumBy (\_ _ -> GT) [1..10] `shouldBe` 10
            myMinimumBy (\_ _ -> LT) [1..10] `shouldBe` 1
            myMinimumBy compare [53, 4, 9001, 10] `shouldBe` 4