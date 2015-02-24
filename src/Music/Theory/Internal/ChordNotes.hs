module Music.Theory.Internal.ChordNotes(getChordNotes) where

import Music.Theory.Internal.Chord
import Music.Theory.Internal.Notes
import Music.Theory.Internal.Intervals

getChordNotes :: String -> [Note]
getChordNotes chordName = do
    let
        rootNote = getRootNote chordName
        symbol = getChordSymbol chordName
        chromatic = [rootNote .. ]
        intervals = getIntervals symbol symbolIntervalsTable
    map (\n -> chromatic !! n) intervals
