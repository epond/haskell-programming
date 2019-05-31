module ReaderSpec where

import Reader
import Test.Hspec

spec :: Spec
spec = do
  describe "Short Exercise: Warming Up" $ do
    it "composed" $ do
      composed "Julie" `shouldBe` "EILUJ"
    it "fmapped" $ do
      fmapped "Chris" `shouldBe` "SIRHC"
    it "tupled" $ do
      tupled "Julie" `shouldBe` ("JULIE","eiluJ")
  describe "Exercise: Reading Comprehension" $ do
    it "myLiftA2" $ do
      (myLiftA2 Dog dogName address) bigbird `shouldBe`
        Dog (DogName "Barkley") (Address "Sesame Street")
