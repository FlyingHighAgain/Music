module Music.Theory.Internal.RomanNumeralAnalysis(analyzeChordProgression) where

import Music.Theory.Internal.Degree
import Music.Theory.Internal.Chord

import Data.String.Utils(replace, split, splitWs)
import Data.List(nub)


toRomanNumeral :: String -> String -> String
toRomanNumeral root chord = degree ++ symbol
  where
    chordRoot = getRootNote chord
    symbol    = getSymbol chord
    degree    = getDegree root chordRoot

--analyzeChordProgression root progression = unwords $ map (toRomanNumeral root) $ words progression

removeBars :: String -> String
removeBars = replace "|" " "

splitChordElements :: String -> [String]
splitChordElements = concat . map (split "/") . splitWs . removeBars

getChordElements :: String -> [String]
getChordElements = nub . splitChordElements

analyzeChordProgression :: String -> String -> String
analyzeChordProgression root progression = foldr (\(old, new) -> replace old new) progression (zip oldList newList)
  where
  	root'   = getRootNote root 
  	oldList = getChordElements progression
  	newList = map (toRomanNumeral root') oldList
