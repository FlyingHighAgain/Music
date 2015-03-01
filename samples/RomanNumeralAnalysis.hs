
import Music.Theory(toRomanNumeral)
import Music.Format.LeadSheet(getMusicKey, filterProgression)

import System.Environment(getArgs)

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
