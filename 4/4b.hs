import Data.Array
import Debug.Trace
main = countRemoved 0 . readarr <$> readFile "input"

countRemoved :: Int -> Array (Int, Int) Char -> Int
countRemoved count arr | trace (show count ++ "\n\n") False = undefined
countRemoved count arr
    | count' == count = count
    | otherwise = countRemoved count' arr' 
    where ixs = filter (accessible arr) (indices arr)
          count' = count + length ixs
          subst = (,'.') <$> ixs
          arr' = arr // subst


readarr :: String -> Array (Int, Int) Char
readarr str = listArray ((1,1), (n,m)) (concat lns)
    where lns = lines str
          n = length lns
          m = length $ head lns

accessible :: Array (Int, Int) Char -> (Int,Int) -> Bool
accessible arr ix = isRoll arr ix && n <= 4
    where n = sum $ fromEnum . isRoll arr <$> range (ix-(1,1), ix+(1,1))
 
isRoll :: Ix i => Array i Char -> i -> Bool
isRoll arr ix =
     inRange (bounds arr) ix && (arr!ix == '@')

instance Num (Int,Int) where
    (a,b)+(c,d) = (a+c,b+d)
    negate (a,b) = (negate a, negate b)
