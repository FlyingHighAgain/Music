
import Music.Theory

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
    describe "Music.Theory.toRomanNumeral" $ do
    	
        it "returns roman numeral chord progression #1" $ do
            toRomanNumeral
               "G" "|G|Am|Bm|C|" `shouldBe` "|I|IIm|IIIm|IV|"

        it "returns roman numeral chord progression #2" $ do
            toRomanNumeral
               "G" "|G Gadd9|Gsus4 G Gadd9 G|" `shouldBe` "|I Iadd9|Isus4 I Iadd9 I|"

        it "returns roman numeral chord progression #3" $ do
            toRomanNumeral
               "Bb" "|Bb FonA|Eb Bb BbonA|Gm Dm7|Eb Fsus4|" `shouldBe` "|I VonVII|IV I IonVII|VIm IIIm7|IV Vsus4|"


--        it "throws an exception if unrecognized scale" $ do
--            evaluate (toRomanNumeral "Xm" "|G|Am|Bm|C|") `shouldThrow` anyException


    describe "Music.Theory.fromRomanNumeral" $ do

        it "returns chord progression from roman numeral progression #1" $ do
            fromRomanNumeral
               "G" "|I|IIm|IIIm|IV|" `shouldBe` "|G|Am|Bm|C|"

        it "returns chord progression from roman numeral progression #2" $ do
            fromRomanNumeral
               "G" "|I Iadd9|Isus4 I Iadd9 I|" `shouldBe` "|G Gadd9|Gsus4 G Gadd9 G|"

        it "returns chord progression from roman numeral progression #3" $ do
            fromRomanNumeral
               "Bb" "|I VonVII|IV I IonVII|VIm IIIm7|IV Vsus4|" `shouldBe` "|Bb FonA|Eb Bb BbonA|Gm Dm7|Eb Fsus4|"




