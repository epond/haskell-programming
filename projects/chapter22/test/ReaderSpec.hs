module ReaderSpec where

import Reader
import Test.Hspec

spec :: Spec
spec = do
  describe "22.2 Short Exercise: Warming Up" $ do
    it "composed" $ do
      composed "Julie" `shouldBe` "EILUJ"
    it "fmapped" $ do
      fmapped "Chris" `shouldBe` "SIRHC"
    it "tupled" $ do
      tupled "Julie" `shouldBe` ("JULIE","eiluJ")
  describe "22.6 Exercise: Reading Comprehension" $ do
    it "myLiftA2" $ do
      (myLiftA2 Dog dogName address) bigbird `shouldBe`
        Dog (DogName "Barkley") (Address "Sesame Street")
  describe "22.7 Exercise: Reader Monad" $ do
    it "helps create a Dog from a Person" $ do
      getDogRM' bigbird `shouldBe`
        Dog (DogName "Barkley") (Address "Sesame Street")
