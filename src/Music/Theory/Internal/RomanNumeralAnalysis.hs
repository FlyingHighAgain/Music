module Music.Theory.Internal.RomanNumeralAnalysis(analyzeChordProgression) where

import Music.Theory.Internal.Degree
import Music.Theory.Internal.Chord


toRomanNumeral :: String -> String -> String
toRomanNumeral root chord = degree ++ symbol
  where
    chordRoot = getRootNote chord
    symbol    = getSymbol chord
    degree    = getDegree root chordRoot

analyzeChordProgression root progression = unwords $ map (toRomanNumeral root) $ words progression
