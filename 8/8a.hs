import Data.List
import Data.Ord

main = product . take 3 . sortBy (comparing Down) . fmap length . circuits . take 1000 . sortOn distance . pairs . fmap readPoint . lines <$> readFile "input"

circuits = foldl conn []

conn circuits (a,b) = 
    case [find (elem p) circuits | p <- [a,b]] of
        [Just c1, Just c2]
            | c1 == c2  -> circuits
            | otherwise -> (c1++c2):(circuits \\ [c1,c2])
        [Just c1, Nothing] -> (b:c1):(delete c1 circuits)
        [Nothing, Just c2] -> (a:c2):(delete c2 circuits)
        [Nothing, Nothing] -> [a,b]:circuits

pairs = pairs' []
pairs' acc [_] = acc 
pairs' acc (h:t) = pairs' (acc ++ fmap (h,) t) t

readPoint ln = read $ "(" ++ ln ++ ")"

distance ((a1,a2,a3), (b1,b2,b3)) = sqrt(fromIntegral $ (b1-a1)^2 + (b2-a2)^2 + (b3-a3)^2)