
import Music.Theory

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
    describe "Music.Theory.toRomanNumeral" $ do
    	
        it "returns roman numeral chord progression #1" $ do
            toRomanNumeral
               (Key G Major) "|G|Am|Bm|C|" `shouldBe` "|I|IIm|IIIm|IV|"

        it "returns roman numeral chord progression #2" $ do
            toRomanNumeral
               (Key G Major) "|G Gadd9|Gsus4 G Gadd9 G|" `shouldBe` "|I Iadd9|Isus4 I Iadd9 I|"

        it "returns roman numeral chord progression #3" $ do
            toRomanNumeral
               (Key Bf Major) "|Bb FonA|Eb Bb BbonA|Gm Dm7|Eb Fsus4|" `shouldBe` "|I VonVII|IV I IonVII|VIm IIIm7|IV Vsus4|"

        it "returns roman numeral chord progression #4" $ do
            toRomanNumeral
               (Key Bf Major) "|Bb F/A|Eb Bb Bb/A|Gm Dm7|Eb Fsus4|" `shouldBe` "|I V/VII|IV I I/VII|VIm IIIm7|IV Vsus4|"


    describe "Music.Theory.fromRomanNumeral" $ do

        it "returns chord progression from roman numeral progression #1" $ do
            fromRomanNumeral
               (Key G Major) "|I|IIm|IIIm|IV|" `shouldBe` "|G|Am|Bm|C|"

        it "returns chord progression from roman numeral progression #2" $ do
            fromRomanNumeral
               (Key G Major) "|I Iadd9|Isus4 I Iadd9 I|" `shouldBe` "|G Gadd9|Gsus4 G Gadd9 G|"

        it "returns chord progression from roman numeral progression #3" $ do
            fromRomanNumeral
               (Key Bf Major) "|I VonVII|IV I IonVII|VIm IIIm7|IV Vsus4|" `shouldBe` "|Bb FonA|Eb Bb BbonA|Gm Dm7|Eb Fsus4|"

        it "returns chord progression from roman numeral progression #4" $ do
            fromRomanNumeral
               (Key Bf Major) "|I V/VII|IV I I/VII|VIm IIIm7|IV Vsus4|" `shouldBe` "|Bb F/A|Eb Bb Bb/A|Gm Dm7|Eb Fsus4|"



