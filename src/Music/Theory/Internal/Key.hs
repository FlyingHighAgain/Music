module Music.Theory.Internal.Key where

import Music.Theory.Internal.Notes
import Music.Theory.Internal.Scales

import Data.String.Utils(endswith)

data Key = Key { getRootNote :: Note, getScale :: Scale } deriving Show

sharpKeys = ["C", "G", "D", "A", "E", "B", "F#", "Am", "Em", "Bm", "F#m", "C#m", "G#m", "D#m"]
flatKeys  = ["F", "Bb", "Eb", "Ab", "Db", "Dm", "Gm", "Cm", "Fm", "Bbm"]

fromKey :: Key -> String
fromKey key = (show $ getRootNote key) ++ if getScale key == Minor then "m" else ""

toKey :: String -> Key
toKey s = Key root scale
  where
    scale = if endswith "m" s then Minor else Major
    root  = if endswith "m" s then read (init s) :: Note else read s

isSharpKey :: Key -> Bool
isSharpKey key = (fromKey key) `elem` sharpKeys

isFlatKey :: Key -> Bool
isFlatKey key = (fromKey key) `elem` flatKeys
