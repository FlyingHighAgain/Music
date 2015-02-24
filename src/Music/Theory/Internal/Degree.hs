module Music.Theory.Internal.Degree where

import Music.Theory.Internal.Notes

import Data.List(elemIndex)
import Data.Maybe(fromJust)


--degrees = ["Ⅰ", "♭Ⅱ", "Ⅱ", "♭Ⅲ", "Ⅲ", "Ⅳ", "♭Ⅴ", "Ⅴ", "♭Ⅵ", "Ⅵ", "♭Ⅶ", "Ⅶ"]
degrees = ["I", "bII", "II", "bIII", "III", "IV", "bV", "V", "bVI", "VI", "bVII", "VII"]

getDegreeIndex :: String -> Int
getDegreeIndex degree = (fromJust . elemIndex degree) degrees

toDegree :: Note -> Note -> String
toDegree rootNote targetNote = degrees !! degreeIndex
  where
    rootIndex   = getNoteIndex rootNote
    targetIndex = getNoteIndex targetNote
    degreeIndex = (targetIndex - rootIndex) `mod` (length degrees)


fromDegree :: Note -> String -> Note
fromDegree rootNote degree = chromatic !! degreeIndex
  where
  	degreeIndex = getDegreeIndex degree
  	chromatic = getChromaticFrom rootNote
