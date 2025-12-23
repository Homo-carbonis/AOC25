import Data.Array

main = countAccessible . readarr <$> readFile "input"

countAccessible :: Array (Int, Int) Char -> Int
countAccessible arr = sum $ fromEnum . accessible arr <$> indices arr

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
    (ix `elem` indices arr) && (arr!ix == '@')

instance Num (Int,Int) where
    (a,b)+(c,d) = (a+c,b+d)
    negate (a,b) = (negate a, negate b)
