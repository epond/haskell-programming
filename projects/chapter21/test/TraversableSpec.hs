module TraversableSpec where

import Traversable
import Test.Hspec

spec :: Spec
spec = do
  describe "21.12 Chapter Exercises" $ do
    it "sanity" $ do
      True `shouldBe` True
