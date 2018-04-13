module HaskellProgramming.Chapter14MorseSpec where

import Test.Hspec
import Test.QuickCheck
import HaskellProgramming.Chapter14Morse
import qualified Data.Map as M

spec :: Spec
spec = do
    describe "Morse" $ do
        it "returns the same character after a roundtrip" $
            property prop_thereAndBackAgain

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

{-
    Example interaction directly in the REPL:
    $ stack ghci haskell-sketchpad:spec
    noise> :set prompt "Haskell> "
    Haskell> import Test.QuickCheck
    Haskell> quickCheck prop_thereAndBackAgain
    +++ OK, passed 100 tests.
-}
prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
    forAll charGen
    (\c -> ((charToMorse c) >>= morseToChar) == Just c)