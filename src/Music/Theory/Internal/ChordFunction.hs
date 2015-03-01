module Music.Theory.Internal.ChordFunction(toChordFunction) where

import Music.Theory.Internal.Scales

import Data.List(nub, sortBy)
import Data.Function(on)
import Data.String.Utils(split, splitWs, replace)

majorTonic = 
    ["I", "I6", "IM7", "I7",
     "IIIm", "IIIm7",
     "bVm7-5",
     "VIm", "VIm7" ]

minorTonic = 
    ["Im", "Im6", "Im7", "ImM7",
     "bIII", "bIII6", "bIIIM7",
     "bVIm7-5"]

majorSubdominant = 
    ["IIm", "IIm7",
     "IV", "IV6", "IVM7", "IV7"]

minorSubdominant = 
    ["bIIM7",
     "IIm7-5",
     "IVm", "IVm6", "IVm7", "IV7", "IVmM7",
     "bVI", "bVI6", "bVIM7", "bVI7",
     "bVII", "bVIIM7"]

majorDominant =
    ["V", "V7", "Vsus4", "V7sus4", "Vm",
     "bII7"]

minorDominant =
    ["V", "V7", "Vsus4", "V7sus4", "Vm",
     "bII7"]

toChordFunctionMajor :: String -> String
toChordFunctionMajor degreeChord = func
  where
    degreeChord' = head $ split "/" $ head $ split "on" degreeChord
    func
      | degreeChord' `elem` majorTonic        = "T"
      | degreeChord' `elem` majorSubdominant  = "S"
      | degreeChord' `elem` majorDominant     = "D"
      | otherwise                             = "X"

toChordFunctionMinor :: String -> String
toChordFunctionMinor degreeChord = func
  where
    degreeChord' = head $ split "/" $ head $ split "on" degreeChord
    func
      | degreeChord' `elem` minorTonic        = "T"
      | degreeChord' `elem` minorSubdominant  = "S"
      | degreeChord' `elem` minorDominant     = "D"
      | otherwise                             = "X"

removeBars :: String -> String
removeBars = replace "|" " "

splitElements :: String -> [String]
splitElements = splitWs . removeBars

getElements :: String -> [String]
getElements = nub . splitElements


toChordFunction :: Scale -> String -> String
toChordFunction scale progression = foldr (\(old, new) -> replace old new) progression (zip oldList newList)
  where
  	oldList = sortBy (compare `on` length) $ getElements progression
  	f = case scale of
  	  Major -> toChordFunctionMajor
  	  Minor -> toChordFunctionMinor
  	newList = map f oldList
