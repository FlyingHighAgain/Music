module Music.Theory.Internal.Intervals where

data NamedIntervals a = NamedIntervals { name :: a, intervals :: [Int]} deriving Show

getIntervals :: (Eq a) => a -> [NamedIntervals a] -> [Int]
getIntervals name1 table  = (intervals . head . filter (\row -> name row == name1)) table

