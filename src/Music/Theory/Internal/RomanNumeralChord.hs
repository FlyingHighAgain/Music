module Music.Theory.Internal.RomanNumeralChord where

import Music.Theory.Internal.Degree

import Data.List(isPrefixOf, maximumBy)
import Data.Function (on)

getRootDegree :: String -> String
getRootDegree romanNumeralChord = maximumBy (compare `on` length) $ filter (\degree -> isPrefixOf degree romanNumeralChord) degrees

getRomanNumeralChordSymbol :: String -> String
getRomanNumeralChordSymbol romanNumeralChord = drop rootLen romanNumeralChord
    where rootLen = (length . getRootDegree) romanNumeralChord
