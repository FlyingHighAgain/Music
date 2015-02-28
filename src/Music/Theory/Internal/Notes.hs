module Music.Theory.Internal.Notes where

import Data.List(elemIndex, isPrefixOf, maximumBy)
import Data.Maybe(fromJust)
import Data.Function (on)

data Note = C | Cs | Df | D | Ds | Ef | E | F | Fs | Gf | G | Gs | Af | A | As | Bf | B

allNotes = [C, Cs, Df, D, Ds, Ef, E, F, Fs, Gf, G, Gs, Af, A, As, Bf, B]
allNotesSig = map show allNotes

getNoteIndex :: Note -> Int
getNoteIndex C  = 0
getNoteIndex Cs = 1
getNoteIndex Df = 1
getNoteIndex D  = 2
getNoteIndex Ds = 3
getNoteIndex Ef = 3
getNoteIndex E  = 4
getNoteIndex F  = 5
getNoteIndex Fs = 6
getNoteIndex Gf = 6
getNoteIndex G  = 7
getNoteIndex Gs = 8
getNoteIndex Af = 8
getNoteIndex A  = 9
getNoteIndex As = 10
getNoteIndex Bf = 10
getNoteIndex B  = 11

instance Eq Note where
  n1 == n2 = getNoteIndex n1 == getNoteIndex n2

instance Ord Note where
  compare n1 n2 = let i1 = getNoteIndex n1; i2 = getNoteIndex n2 in compare i1 i2

instance Enum Note where
  fromEnum n = getNoteIndex n
  toEnum   i = allNotes !! index
    where
      indices = map getNoteIndex allNotes
      index = fromJust $ elemIndex (i `mod` 12) indices

instance Show Note where
  show C  = "C"
  show Cs = "C#"
  show Df = "Db"
  show D  = "D"
  show Ds = "D#"
  show Ef = "Eb"
  show E  = "E"
  show F  = "F"
  show Fs = "F#"
  show Gf = "Gb"
  show G  = "G"
  show Gs = "G#"
  show Af = "Ab"
  show A  = "A"
  show As = "A#"
  show Bf = "Bb"
  show B  = "B"

instance Read Note where
  readsPrec _ value = [(note, rest)]
    where
      allNotes' = map show allNotes
      eaten = maximumBy (compare `on` length) $ filter (\note -> isPrefixOf note value) allNotes'
      rest  = drop (length eaten) value
      pairList = map (\n -> (show n, n)) allNotes
      note = fromJust $ lookup eaten pairList


getChromaticFrom :: Note -> [Note]
getChromaticFrom note = [note .. ]

readNote :: String -> Note
readNote s = (read s) :: Note

toFlatNote :: Note -> Note
toFlatNote Cs = Df
toFlatNote Ds = Ef
toFlatNote Fs = Gf
toFlatNote Gs = Af
toFlatNote As = Bf
toFlatNote n  = n

toSharpNote :: Note -> Note
toSharpNote Df = Cs
toSharpNote Ef = Ds
toSharpNote Gf = Fs
toSharpNote Af = Gs
toSharpNote Bf = As
toSharpNote n  = n





