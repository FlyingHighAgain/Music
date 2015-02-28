--module Music.Theory.Internal.RomanNumeral(toRomanNumeral) where
module Music.Theory.Internal.RomanNumeral where

import Music.Theory.Internal.Notes
import Music.Theory.Internal.Scale
import Music.Theory.Internal.Degree
import Music.Theory.Internal.Chord
import Music.Theory.Internal.RomanNumeralChord

import Data.String.Utils(replace, split, splitWs)
import Data.List(nub, sortBy)
import Data.Function(on)

toRomanNumeralChord :: Note -> String -> String
toRomanNumeralChord root chord = degree ++ symbol
  where
    chordRoot = getRootNote chord
    symbol    = getChordSymbol chord
    degree    = toDegree root chordRoot

fromRomanNumeralChord :: String -> String -> String
fromRomanNumeralChord key romanNumeralChord = chordRoot ++ symbol
  where
  	degree    = getRootDegree romanNumeralChord
  	symbol    = getRomanNumeralChordSymbol romanNumeralChord
  	keyNote   = getRootNote key
  	rootNote  = fromDegree keyNote degree
  	isSharp   = isSharpScale key
  	chordRoot = show $ if isSharp then toSharpNote rootNote else toFlatNote rootNote

removeBars :: String -> String
removeBars = replace "|" " "

splitChordElements :: String -> [String]
splitChordElements = concat . map (split "on") . splitWs . removeBars

getChordElements :: String -> [String]
getChordElements = nub . splitChordElements

toRomanNumeral :: String -> String -> String
toRomanNumeral key progression = foldr (\(old, new) -> replace old new) progression (zip oldList newList)
  where
  	root   = getRootNote key 
  	oldList = sortBy (compare `on` length) $ getChordElements progression
  	newList = map (toRomanNumeralChord root) oldList

fromRomanNumeral :: String -> String -> String
fromRomanNumeral key progression = foldr (\(old, new) -> replace old new) progression (zip oldList newList)
  where
  	oldList = sortBy (compare `on` length) $ getChordElements progression
  	newList = map (fromRomanNumeralChord key) oldList

