import Data.List
main =  sum . fmap (calc . reverse) . unzipl . fmap words . lines <$> readFile "input"

calc :: [String] -> Int
calc ("+":xs) = sum $ read <$> xs
calc ("*":xs) = product $ read <$> xs


unzipl = unfoldr unconsl 

unconsl lst
    | null $ head lst = Nothing
    | otherwise = Just (head <$> lst, tail <$> lst)
