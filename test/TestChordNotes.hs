
import Music.Theory

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
    describe "Music.Theory.getChordNotes" $ do
        it "returns C notes" $ do
            getChordNotes "C" `shouldBe` [C, E, G]

        it "throws an exception if unrecognized chord name" $ do
            evaluate (getChordNotes "Xm7") `shouldThrow` anyException



