
import Music.Theory

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
    describe "Music.Theory.getChordNotes" $ do
    	
        it "returns C notes" $ do
            getChordNotes "C" `shouldBe` [C, E, G]

        it "returns B notes" $ do
            getChordNotes "B" `shouldBe` [B, Ds, Fs]

        it "returns F# notes" $ do
            getChordNotes "F#" `shouldBe` [Fs, As, Cs]

        it "returns Cm notes" $ do
            getChordNotes "Cm" `shouldBe` [C, Ef, G]

        it "returns C7 notes" $ do
            getChordNotes "C7" `shouldBe` [C, E, G, Bf]

        it "returns CM7 notes" $ do
            getChordNotes "CM7" `shouldBe` [C, E, G, B]

        it "returns CmM7 notes" $ do
            getChordNotes "CmM7" `shouldBe` [C, Ef, G, B]

        it "returns Cdim notes" $ do
            getChordNotes "Cdim" `shouldBe` [C, Ef, Gf, A]

        it "returns Cm7-5 notes" $ do
            getChordNotes "Cm7-5" `shouldBe` [C, Ef, Gf, Bf]

        it "returns Caug notes" $ do
            getChordNotes "Caug" `shouldBe` [C, E, Gs]

        it "returns Csus4 notes" $ do
            getChordNotes "Csus4" `shouldBe` [C, F, G]

        it "returns C7sus4 notes" $ do
            getChordNotes "C7sus4" `shouldBe` [C, F, G, Bf]

        it "returns C6 notes" $ do
            getChordNotes "C6" `shouldBe` [C, E, G, A]

        it "returns C9 notes" $ do
            getChordNotes "C9" `shouldBe` [C, E, G, Bf, D]

        it "returns C11 notes" $ do
            getChordNotes "C11" `shouldBe` [C, E, G, Bf, D, F]

        it "returns C13 notes" $ do
            getChordNotes "C13" `shouldBe` [C, E, G, Bf, D, F, A]

        it "throws an exception if unrecognized chord name" $ do
            evaluate (getChordNotes "Xm7") `shouldThrow` anyException



