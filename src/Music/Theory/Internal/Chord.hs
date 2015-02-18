module Music.Theory.Internal.Chord where

import Music.Theory.Internal.Common

import Data.List(isPrefixOf, maximumBy)
import Data.Function (on)

symbolIntervalsTable = [
    NamedIntervals ""      [0, 4, 7],
    NamedIntervals "m"     [0, 3, 7],
    NamedIntervals "7"     [0, 4, 7, 10],
    NamedIntervals "M7"    [0, 4, 7, 11],
    NamedIntervals "m7"    [0, 3, 7, 10],
    NamedIntervals "mM7"   [0, 3, 7, 11],
    NamedIntervals "dim"   [0, 3, 6, 9],
    NamedIntervals "m7-5"  [0, 3, 6, 10],
    NamedIntervals "aug"   [0, 4, 8],
    NamedIntervals "sus4"  [0, 5, 7],
    NamedIntervals "7sus4" [0, 5, 7, 10],
    NamedIntervals "6"     [0, 4, 7, 9],
    NamedIntervals "9"     [0, 4, 7, 10, 14],
    NamedIntervals "11"    [0, 4, 7, 10, 14, 17],
    NamedIntervals "13"    [0, 4, 7, 10, 14, 17, 21]]

getRootNote :: String -> String
getRootNote chordName = maximumBy (compare `on` length) $ filter (\note -> isPrefixOf note chordName) allNotes

getSymbol :: String -> String
getSymbol chordName = drop rootLen chordName
    where rootLen = (length . getRootNote) chordName
