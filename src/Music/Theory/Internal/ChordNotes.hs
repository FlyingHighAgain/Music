module Music.Theory.Internal.ChordNotes(getChordNotes) where

import Music.Theory.Internal.Chord
import Music.Theory.Internal.Notes
import Music.Theory.Internal.Intervals

getChordNotes :: String -> [[String]]
getChordNotes chordName = do
    let
        root = getRootNote chordName
        symbol = getChordSymbol chordName
        chromatic = getChromaticFrom root
        intervals = getIntervals symbol symbolIntervalsTable
    map (\n -> chromatic !! n) intervals
