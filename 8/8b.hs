import Data.List

main = circuits . fmap readPoint . lines <$> readFile "input"

circuits points = circuits' ((0,0,0),(0,0,0)) (fmap singleton points) (sortOn distance (pairs points))
circuits' ((a1,_,_),(b1,_,_)) [c] _ = a1 * b1
circuits' _ acc (h:t) = circuits' h (conn acc h) t

conn circuits (a,b)
    | c1 == c2  = circuits
    | otherwise = (c1++c2):(circuits \\ [c1,c2])
    where [Just c1, Just c2] = [find (elem p) circuits | p <- [a,b]]

pairs = pairs' []
pairs' acc [_] = acc
pairs' acc (h:t) = pairs' (acc ++ fmap (h,) t) t

readPoint ln = read $ "(" ++ ln ++ ")"

distance ((a1,a2,a3), (b1,b2,b3)) = sqrt (fromIntegral $ (b1-a1)^2 + (b2-a2)^2 + (b3-a3)^2)

