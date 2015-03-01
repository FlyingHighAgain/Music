
import Music.Theory

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
    describe "Music.Theory.getScaleNotes" $ do
    	
        it "returns C major scale" $ do
            getScaleNotes C Major `shouldBe` [C, D, E, F, G, A, B]

        it "returns C minor scale" $ do
            getScaleNotes C Minor `shouldBe` [C, D, Ef, F, G, Af, Bf]

