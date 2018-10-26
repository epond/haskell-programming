module MonadsSpec where

import Monads
import Test.Hspec

spec :: Spec
spec = do
    describe "Sanity check" $ do
        it "is sane" $ do
            True `shouldBe` True
