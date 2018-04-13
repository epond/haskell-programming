module Chapter16FunctorsSpec where

import Test.Hspec
import Test.QuickCheck
import Chapter16Functors

spec :: Spec
spec = do
    describe "QuickCheck" $ do
        it "can check the Functor identity law" $ do
            property ((\x -> functorIdentity x) :: [Int] -> Bool)
        it "can check the Functor composition law" $ do
            property (\x -> (functorCompose (+1) (*2)) (x :: [Int]))