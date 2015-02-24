module Music.Theory.Internal.Scale where

sharpScales = ["C", "G", "D", "A", "E", "B", "F#", "Am", "Em", "Bm", "F#m", "C#m", "G#m", "D#m"]
flatScales  = ["F", "Bb", "Eb", "Ab", "Db", "Dm", "Gm", "Cm", "Fm", "Bbm"]

isSharpScale :: String -> Bool
isSharpScale key = key `elem` sharpScales

isFlatScale :: String -> Bool
isFlatScale key = key `elem` flatScales
