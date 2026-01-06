import Data.List
import Data.Ord

main = maximum . fmap area . pairs . fmap readPoint . lines <$> readFile "input"

pairs = pairs' []
pairs' acc [_] = acc 
pairs' acc (h:t) = pairs' (acc ++ fmap (h,) t) t

readPoint ln = read $ "(" ++ ln ++ ")"

area ((a1,a2), (b1,b2)) = (abs (b1-a1) + 1) * (abs (b2-a2) + 1)
