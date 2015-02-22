module Music.Theory.Internal.Notes where

import Data.List(findIndex)
import Data.Maybe(fromJust)


{-}
midiNumbers = [0..127]
noteNo = zip midiNumbers $ cycle notes
-}

notes = [["C"],["C#","Db"],["D"],["D#","Eb"],["E"],["F"],["F#","Gb"],["G"],["G#","Ab"],["A"],["A#","Bb"],["B"]]
allNotes = concat notes

sharpScales = ["C", "G", "D", "A", "E", "B", "F#", "Am", "Em", "Bm", "F#m", "C#m", "G#m", "D#m"]
flatScales  = ["F", "Bb", "Eb", "Ab", "Db", "Dm", "Gm", "Cm", "Fm", "Bbm"]

isSharpScale :: String -> Bool
isSharpScale key = key `elem` sharpScales

isFlatScale :: String -> Bool
isFlatScale key = key `elem` flatScales

getNoteIndex :: String -> Int
getNoteIndex note = (fromJust . findIndex (\n -> elem note n)) notes

getChromaticFrom :: String -> [[String]]
getChromaticFrom note = drop index $ cycle notes
    where index = getNoteIndex note
