module HaskellProgramming.Chapter06TypeclassesSpec where

import Test.Hspec
import HaskellProgramming.Chapter06Typeclasses

spec :: Spec
spec = do
    describe "Typeclasses" $ do
        it "has an Eq instance for TisAnInteger" $ do
            TisAn 2 == TisAn 2 `shouldBe` True
            TisAn 2 == TisAn 3 `shouldBe` False
        it "has an Eq instance for TwoIntegers" $ do
            Two 7 8 == Two 7 8 `shouldBe` True
            Two 7 8 == Two 7 9 `shouldBe` False
            Two 7 8 == Two 8 8 `shouldBe` False
        it "has an Eq instance for StringOrInt" $ do
            TisAnInt 5 == TisAnInt 5 `shouldBe` True
            TisAString "abc" == TisAString "abc" `shouldBe` True
            TisAnInt 5 == TisAnInt 4 `shouldBe` False
            TisAString "abc" == TisAString "abb" `shouldBe` False
            TisAnInt 5 == TisAString "5" `shouldBe` False
        it "has an Eq instance for Pair" $ do
            Pair 4 7 == Pair 4 7 `shouldBe` True
            Pair 4 7 == Pair 4 8 `shouldBe` False
            Pair 4 7 == Pair 5 7 `shouldBe` False
        it "has an Eq instance for Tuple" $ do
            Tuple 4 7 == Tuple 4 7 `shouldBe` True
            Tuple 4 "7" == Tuple 4 "7" `shouldBe` True
            Tuple 4 "7" == Tuple 4 "8" `shouldBe` False
            Tuple 4 "7" == Tuple 5 "7" `shouldBe` False
        it "has an Eq instance for Which" $ do
            ThisOne 4 == ThisOne 4 `shouldBe` True
            ThisOne 4 == ThisOne 5 `shouldBe` False
            ThatOne 4 == ThatOne 4 `shouldBe` True
            ThatOne 4 == ThatOne 5 `shouldBe` False
            ThisOne 4 == ThatOne 4 `shouldBe` False
            ThatOne 4 == ThisOne 4 `shouldBe` False
        it "has an Eq instance for EitherOr" $ do
            (Hello 4 :: EitherOr Int Int) == (Hello 4 :: EitherOr Int Int) `shouldBe` True
            (Hello 4 :: EitherOr Int Int) == (Hello 5 :: EitherOr Int Int) `shouldBe` False
            (Hello 4 :: EitherOr Int Int) == (Goodbye 4 :: EitherOr Int Int) `shouldBe` False
            (Goodbye 4 :: EitherOr Int Int) == (Goodbye 4 :: EitherOr Int Int) `shouldBe` True
            (Goodbye 4 :: EitherOr Int Int) == (Goodbye 5 :: EitherOr Int Int) `shouldBe` False
