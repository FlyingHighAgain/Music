module Music.Theory.Internal.Common where

import Data.List(findIndex)
import Data.Maybe(fromJust)

notes = [["C"],["C#","Db"],["D"],["D#","Eb"],["E"],["F"],["F#","Gb"],["G"],["G#","Ab"],["A"],["A#","Bb"],["B"]]
allNotes = concat notes

{-}
midiNumbers = [0..127]
noteNo = zip midiNumbers $ cycle notes
-}

data NamedIntervals = NamedIntervals { name :: String, intervals :: [Int]} deriving Show

getIntervals :: String ->[NamedIntervals] -> [Int]
getIntervals name1 table  = (intervals . head . filter (\row -> name row == name1)) table

getNoteIndex :: String -> Int
getNoteIndex note = (fromJust . findIndex (\n -> elem note n)) notes

getChromaticFrom :: String -> [[String]]
getChromaticFrom note = drop index $ cycle notes
    where index = getNoteIndex note
