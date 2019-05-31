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
