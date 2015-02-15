module Music.Theory(getCordNotes) where

import Data.List(isPrefixOf, findIndex, maximumBy)
import Data.Maybe(fromJust)
import Data.Function (on)

notes = [["A"],["A#","Bb"],["B"],["C"],["C#","Db"],["D"],["D#","Eb"],["E"],["F"],["F#","Gb"],["G"],["G#","Ab"]]

allNotes = concat notes

symbolIntervalSets = [
    ("",      [0, 4, 7]),
    ("m",     [0, 3, 7]),
    ("7",     [0, 4, 7, 10]),
    ("M7",    [0, 4, 7, 11]),
    ("m7",    [0, 3, 7, 10]),
    ("mM7",   [0, 3, 7, 11]),
    ("dim",   [0, 3, 6, 9]),
    ("m7-5",  [0, 3, 6, 10]),
    ("aug",   [0, 4, 8]),
    ("sus4",  [0, 5, 7]),
    ("7sus4", [0, 5, 7, 10]),
    ("6",     [0, 4, 7, 9]),
    ("9",     [0, 4, 7, 10, 14]),
    ("11",    [0, 4, 7, 10, 14, 17]),
    ("13",    [0, 4, 7, 10, 14, 17, 21])]

getRootNote :: String -> String
getRootNote cordName = maximumBy (compare `on` length) $ filter (\note -> isPrefixOf note cordName) allNotes

getSymbol :: String -> String
getSymbol cordName = drop rootLen cordName
    where rootLen = (length . getRootNote) cordName

getIntervals :: String -> [Int]
getIntervals cordName = (snd . head . filter (\symbolIntervalSet -> fst symbolIntervalSet == symbol)) symbolIntervalSets
    where symbol = getSymbol cordName

getCordNotes :: String -> [[String]]
getCordNotes cordName = do
    let
        root = getRootNote cordName
        index = (fromJust . findIndex (\note -> elem root note)) notes
        chromatic = drop index $ cycle notes
        intervals = getIntervals cordName
    map (\n -> chromatic !! n) intervals

