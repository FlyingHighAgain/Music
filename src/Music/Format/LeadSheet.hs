module Music.Format.LeadSheet(
    getMusicKey,
    filterProgression
    ) where

import Music.Theory(Key, toKey)

import Data.String.Utils(startswith, replace)
import Text.Regex(mkRegex, subRegex)

getInfoLine :: String -> String -> String
getInfoLine key contents = head $ filter (startswith ('%':key)) $ lines contents

getMusicKey :: String -> Key
getMusicKey contents = key
  where
    infoKey = "KEY"
    keyLine = getInfoLine infoKey contents
    value'  = subRegex (mkRegex ('%':infoKey ++ "[[:space:]]*=[[:space:]]*")) keyLine ""
    value'' = replace "\"" "" value'
    key     = toKey value''

filterProgression :: String -> String
filterProgression = unlines . filterChordProgression . lines
  where
    abstructProgressionLines = (filter (startswith "|"))
    removeEmbeddedInstruction line = subRegex (mkRegex "<.*>") line ""
    removeNoteValue line = subRegex (mkRegex ":[[:digit:].~]*") line ""
    filterChordProgression = map (removeNoteValue . removeEmbeddedInstruction) . abstructProgressionLines
