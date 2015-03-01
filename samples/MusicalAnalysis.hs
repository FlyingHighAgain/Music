
import Music.Theory(toRomanNumeral, toChordFunction, Key( .. ))
import Music.Format.LeadSheet(getMusicKey, filterProgression)

import System.Environment(getArgs)

main :: IO()
main = do
  args <- getArgs
  let
    input = if (null args) then getContents  else readFile (head args)
  contents <- input
  let
    key = getMusicKey contents
    scale = getScale key

    progression = filterProgression contents
    romanProgression = toRomanNumeral key progression
    funcProgression = toChordFunction scale romanProgression

    lineSets = zipWith3 ( \a b c -> a ++ "\n" ++ b ++ "\n" ++ c ++ "\n" ) (lines progression) (lines romanProgression) (lines funcProgression)

  putStr $ unlines lineSets

