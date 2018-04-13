module CipherSpec where

import Test.Hspec
import Cipher

spec :: Spec
spec = do
    describe "Caesar Cipher" $ do
        it "encodes" $ do
            caesar 3 "abc" `shouldBe` "def"
        it "decodes" $ do
            unCaesar 3 "def" `shouldBe` "abc"
    describe "Vigenere Cipher" $ do
        it "can rotate forward by character" $ do
            rotByChar (flip (-)) 'a' 'a' `shouldBe` 'a'
            rotByChar (flip (-)) 'c' 'a' `shouldBe` 'c'
            rotByChar (flip (-)) 'c' 'd' `shouldBe` 'f'
            rotByChar (flip (-)) 'd' 'y' `shouldBe` 'b'
        it "can rotate backward by character" $ do
            rotByChar (-) 'a' 'a' `shouldBe` 'a'
            rotByChar (-) 'c' 'c' `shouldBe` 'a'
            rotByChar (-) 'c' 'f' `shouldBe` 'd'
            rotByChar (-) 'd' 'b' `shouldBe` 'y'
        it "encodes" $ do
            vigenere "ally" "meetatdawn" `shouldBe` "mppraeoywy"
        it "decodes" $ do
            unVigenere "ally" "mppraeoywy" `shouldBe` "meetatdawn"