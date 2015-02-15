module Music.Theory(getScaleNotes) where

import Data.List(findIndex)
import Data.Maybe(fromJust)

midiNumbers = [0..127]
notes = [["C"],["C#","Db"],["D"],["D#","Eb"],["E"],["F"],["F#","Gb"],["G"],["G#","Ab"],["A"],["A#","Bb"],["B"]]
noteNo = zip midiNumbers $ cycle notes

scaleIntervalSets = [
    ("major",     [0, 2, 4, 5, 7, 9, 11]),
    ("minor",     [0, 2, 3, 5, 7, 8, 10])]

getKeyNo :: String -> Int
getKeyNo key = (fromJust . findIndex (\note -> elem key note)) notes

getIntervals :: String -> [Int]
getIntervals scale = (snd . head . filter (\symbolIntervalSet -> fst symbolIntervalSet == scale)) scaleIntervalSets

getScaleNotes :: String -> String -> [[String]]
getScaleNotes key scale = do
    let
        index = getKeyNo key
        chromatic = drop index $ cycle notes
        intervals = getIntervals scale
    map (\n -> chromatic !! n) intervals


