
import Music.Theory

import System.Environment(getArgs)
import Data.String.Utils(startswith, replace)
import Text.Regex(mkRegex, subRegex)


getInfoLine :: String -> String -> String
getInfoLine key contents = head $ filter (startswith ('%':key)) $ lines contents

getMusicKey :: String -> String
getMusicKey contents = value
  where
    infoKey = "KEY"
    keyLine = getInfoLine infoKey contents
    value'  = subRegex (mkRegex ('%':infoKey ++ "[[:space:]]*=[[:space:]]*")) keyLine ""
    value   = replace "\"" "" value'

filterProgression :: String -> String
filterProgression = unlines . filterChordProgression . lines
  where
    abstructProgressionLines = (filter (startswith "|"))
    removeEmbeddedInstruction line = subRegex (mkRegex "<.*>") line ""
    removeNoteValue line = subRegex (mkRegex ":[[:digit:].~]*") line ""
    filterChordProgression = map (removeNoteValue . removeEmbeddedInstruction) . abstructProgressionLines


main :: IO()
main = do
  args <- getArgs
  let
    input = if (length args > 0)  then readFile (args !! 0) else getContents 
  contents <- input
  let
    key = getMusicKey contents
    progression = filterProgression contents
    converted = toRomanNumeral key progression
  putStr converted
