module Music.Theory.Internal.Intervals where

data NamedIntervals = NamedIntervals { name :: String, intervals :: [Int]} deriving Show

getIntervals :: String ->[NamedIntervals] -> [Int]
getIntervals name1 table  = (intervals . head . filter (\row -> name row == name1)) table

