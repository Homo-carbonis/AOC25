import Data.List
import Data.Maybe
main = suml 0 . tokBy (all (==' ')) . unzipl . lines <$> readFile "input"

suml acc [] = acc
suml acc (h:t) = suml (acc + result) t
    where op = case last $ head h of
                   '+' -> sum
                   '*' -> product
          result = op $ read . init <$> h

tokBy :: (a -> Bool) -> [a] -> [[a]]
tokBy f s = case dropWhile f s of
  [] -> []
  s' -> w : tokBy f s''
    where
      (w, s'') = break f s'

unzipl = unfoldr unconsl

unconsl lst
    | null $ head lst = Nothing
    | otherwise = Just (head <$> lst, tail <$> lst)
