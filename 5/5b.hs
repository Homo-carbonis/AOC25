import Data.List
main = countFresh . readRanges <$> readFile "input"

countFresh :: [(Int,Int)] -> Int
countFresh = countFresh' 0 . sortOn fst

countFresh' :: (Ord t, Num t) => t -> [(t, t)] -> t
countFresh' acc ((a,b):(c,d):t)
    | b < c = countFresh' (acc + (b - a) + 1) ((c,d):t)
    | b <= d = countFresh' acc ((a,d):t)
    | b > d =  countFresh' acc ((a,b):t)

countFresh' acc [(a,b)] = acc + (b - a) + 1
          
readRanges :: String -> [(Int, Int)]
readRanges str = readRange <$> takeWhile (/="") (lines str)   

readRange :: (Read a, Read b) => [Char] -> (a, b)
readRange str = (read a, read b)
    where (a, _:b) = break (=='-') str