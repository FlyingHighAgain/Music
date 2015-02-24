module Music.Theory.Internal.ScaleNotes(getScaleNotes) where

import Music.Theory.Internal.Notes
import Music.Theory.Internal.Intervals


scaleIntervalsTable = [
    NamedIntervals "major" [0, 2, 4, 5, 7, 9, 11],
    NamedIntervals "minor" [0, 2, 3, 5, 7, 8, 10]]

getScaleNotes :: Note -> String -> [Note]
getScaleNotes key scale = do
    let
        index = getNoteIndex key
        chromatic = getChromaticFrom key
        intervals = getIntervals scale scaleIntervalsTable
    map (\n -> chromatic !! n) intervals
