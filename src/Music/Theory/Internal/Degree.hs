module Music.Theory.Internal.Degree where

import Music.Theory.Internal.Common


--degrees = ["Ⅰ", "♭Ⅱ", "Ⅱ", "♭Ⅲ", "Ⅲ", "Ⅳ", "♭Ⅴ", "Ⅴ", "♭Ⅵ", "Ⅵ", "♭Ⅶ", "Ⅶ"]
degrees = ["I", "bII", "II", "bIII", "III", "IV", "bV", "V", "bVI", "VI", "bVII", "VII"]

getDegree :: String -> String -> String
getDegree rootNote targetNote = degrees !! degreeIndex
  where
    rootIndex   = getNoteIndex rootNote
    targetIndex = getNoteIndex targetNote
    degreeIndex = (targetIndex - rootIndex) `mod` (length degrees)
