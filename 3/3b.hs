import System.IO
import Data.List
import Data.Maybe 

main = sum . fmap (maximumJoltage 12) . lines <$> readFile "input"

maximumJoltage :: Int -> String -> Int
maximumJoltage = maximumJoltage' ""

maximumJoltage' :: String -> Int -> String -> Int
maximumJoltage' acc 0 str = read $ reverse acc
maximumJoltage' acc n str = maximumJoltage' (digit:acc) (n-1) str' 
    where digit = maximum $ dropr (n-1) str
          str' = drop (fromJust (elemIndex digit str) + 1) str


dropr n str = take m str
    where m = length str - n
    
    -- read $ unfoldr f str
    -- where d1 = maximum $ init str
    --       d2 = maximum $ drop (fromJust (elemIndex d1 str) + 1) str
