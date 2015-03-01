--module Music.Theory.Internal.RomanNumeral(toRomanNumeral) where
module Music.Theory.Internal.RomanNumeral where

import Music.Theory.Internal.Notes
import Music.Theory.Internal.Scales
import qualified Music.Theory.Internal.Key as K
import Music.Theory.Internal.Degree
import Music.Theory.Internal.Chord
import Music.Theory.Internal.RomanNumeralChord

import Data.String.Utils(replace, split, splitWs)
import Data.List(nub, sortBy)
import Data.Function(on)

toRomanNumeralChord :: K.Key -> String -> String
toRomanNumeralChord key chord = degree ++ symbol
  where
    keyNote   = K.getRootNote key
    chordRoot = getRootNote chord
    symbol    = getChordSymbol chord
    degree    = toDegree keyNote chordRoot

fromRomanNumeralChord :: K.Key -> String -> String
fromRomanNumeralChord key romanNumeralChord = chordRoot ++ symbol
  where
    keyNote   = K.getRootNote key
    degree    = getRootDegree romanNumeralChord
    symbol    = getRomanNumeralChordSymbol romanNumeralChord
    root      = fromDegree keyNote degree
    isSharp   = K.isSharpKey key
    chordRoot = show $ if isSharp then toSharpNote root else toFlatNote root

removeBars :: String -> String
removeBars = replace "|" " "

splitChordElements :: String -> [String]
splitChordElements = concat . map (split "/") . concat . map (split "on") . splitWs . removeBars

getChordElements :: String -> [String]
getChordElements = nub . splitChordElements

toRomanNumeral :: K.Key -> String -> String
toRomanNumeral key progression = foldr (\(old, new) -> replace old new) progression (zip oldList newList)
  where
  	oldList = sortBy (compare `on` length) $ getChordElements progression
  	newList = map (toRomanNumeralChord key) oldList

fromRomanNumeral :: K.Key -> String -> String
fromRomanNumeral key progression = foldr (\(old, new) -> replace old new) progression (zip oldList newList)
  where
    oldList = sortBy (compare `on` length) $ getChordElements progression
    newList = map (fromRomanNumeralChord key) oldList

